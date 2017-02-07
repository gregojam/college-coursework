import static java.lang.Math.toIntExact;

import java.lang.reflect.Method;
import java.util.List;
import java.util.LinkedList;

import javax.swing.SwingUtilities;

/**
 * Game is a controller that run a game in either interactive or batch mode. Unless 
 * resized, the game is played on a board of size Constants.DEFAULT_SIZE.
 * 
 * Operations are provided to compare run times of different flood functions 
 * (using reflection to automatically detect the defined methods in the Board class).
 */

public class Game {
  private int size = Constants.DEFAULT_SIZE;
  private Board board;
  private GUI theView;
  private boolean interactive = true;

  private int currentStep;
  private List<Integer> thisRun = new LinkedList<>();  // for timings when running in batch mode

  /**
   * Runs a game in interactive mode. See comment for how to run in batch (i.e., testing) mode.
   */
  public static void main(final String... args) {
    System.out.println(Constants.TITLE);
    // Run a game in interactive mode:
    SwingUtilities.invokeLater(() -> new Game());
    
    // Uncomment the following line to run a batch of games and display a graph of the timings:
    //new Game(false).batchTest();
  }

  /**
   * Creates an interactive game.
   */
  public Game() {
    this(true);
  }

  /**
   * Creates either an interactive or simulated (autoplayed) game depending on the
   * sense of the argument.
   */
  public Game(boolean interactive) {
    this.interactive = interactive;
    init();
  }
  
  /**
   * Returns the board associated with this game.
   */
  
  public Board getBoard() {
    return board;
  }

  /**
   * Updates the board size and restarts.
   */
  
  void resize(int size) {
    this.size = size;
    if (interactive) 
      theView.dispose();
    init();
  }

  /**
   * Initializes this game to a fresh state and starts up the gui.
   */
  
  private void init() {
    board = new Board(size);
    currentStep = 0;
    if (interactive)
      theView = new GUI(this);
  }

  /**
   * Returns true iff the player has run out of steps.
   */
  
  public boolean noMoreSteps() {
    return currentStep == getStepLimit();
  }
  
  /**
   * Returns the number of steps used by the player so far during this game.
   */
  
  public int getSteps() {
    return currentStep;
  }
  
  /**
   * Returns the maximum number of steps for this game.
   */
  
  public int getStepLimit() {
    return size * 25 / 14 + 1;
  }

  /**
   * Processes one step of the game (where the player has selected the given color
   * for their move) using the standard flood function.
   */
  
  public void select(WaterColor color) {
    select(0, color); // k == 0 means to use Board.flood() as the flood function
  }

  /**
   * Processes one stop of the game (where the player has selected the give color
   * for their move) using the kth flood function (where k = 0, 1, 2, ...)
   */
  
  public void select(int k, WaterColor color) {
    try {
      Class<?> c = Board.class; 
      Class<?>[] argTypes = new Class[] { WaterColor.class };
      String name = "flood" + (k == 0 ? "" : k);
      Method floodFunction = c.getDeclaredMethod(name, argTypes);
      currentStep++;
      floodFunction.invoke(board, color);  
    } 
    catch (Exception e) {
      System.out.println("Something went terribly wrong with the flood reflection!");
      e.printStackTrace();
    }
  }

  /**
   * Plays a series of games in batch mode, where player moves are selected 
   * according to the board's suggestions, and the kth flood function is used,
   * and adds the averaged elapsed time to the thisRun list. 
   */
  
  private void autoPlay(int k) {
    long gameTime = 0;
    for (int i = 0; i < Constants.NUM_GAMES_TO_AUTOPLAY; i++) {
      long startTime = System.currentTimeMillis();
      while (!board.fullyFlooded())
        select(k, board.suggest2());
      long endTime = System.currentTimeMillis();
      gameTime += (endTime - startTime);
      init();
    }
    gameTime /= Constants.NUM_GAMES_TO_AUTOPLAY;
    // System.out.println(total);
    thisRun.add(toIntExact(gameTime));
  }

  /**
   * Runs a batch of tests, on boards of varying sizes, through autoPlay(), iterating over 
   * all defined flood functions, and then displays a graph of run times.
   */
  
  private void batchTest() {
    List<List<Integer>> allTimings = new LinkedList<>();
    Class<?> c = Board.class; 
    Class<?>[] argTypes = new Class[] { WaterColor.class };
    int k = 0;
    try {
      while (true) {
        String name = "flood" + (k == 0 ? "" : k);
        c.getDeclaredMethod(name, argTypes); // throws exception if no such method
        System.out.println("running with " + name + " as the flood function");
        thisRun = new LinkedList<>();
        for (int size = 1; size <= Constants.MAX_BOARD_SIZE_FOR_AUTOPLAY; size++) {
          //System.out.println("testing a board of size " + size);
          resize(size);
          autoPlay(k);  // use the kth flood function
        }
        System.out.println("timings for the above boards: " + thisRun);
        allTimings.add(thisRun);
        k++;
      }
    }
    catch (NoSuchMethodException e) {
      // All done! No more flood functions left to try.
    }
    finally {
      SwingUtilities.invokeLater(() -> new TimingGraph(allTimings));
    }
  }
}

