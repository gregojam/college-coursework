// N-Puzzle
// James Gregory    gregojam
// "Programming Assignment 105"
// "A290/A590 / Fall 2017"
// 10-9-17

// GameViewController.swift

import UIKit

class GameViewController: UIViewController {
    
    let appDelegate = UIApplication.shared.delegate as? AppDelegate
    var nPuzzle : NPuzzleModel?
    var users : [String: UserModel]?
    var currentUser : String?
    var loadedUser : String?
    
    
    var buttons : [[MyTileButton]] = [[],[],[]]
    var subview : UIView = UIView()
    
    @IBOutlet var victoryLabel : UILabel!
    
    @IBAction func newGame() {
        self.nPuzzle = appDelegate?.nPuzzle
        nPuzzle!.shuffle()
        
        for i in 0...2 {
            for j in 0...2 {
                let button = buttons[j][i]
                button.setTitle(String(describing: nPuzzle!.board[i][j]), for: .normal)
                if button.currentTitle == "0" {
                    button.isHidden = true
                }
                else {
                    button.isHidden = false
                }
                button.isUserInteractionEnabled = true
            }
        }
        victoryLabel.isHidden = true
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        self.nPuzzle = self.appDelegate?.nPuzzle
        self.users = self.appDelegate?.users
        self.currentUser = self.appDelegate?.currentUser
        self.loadedUser = currentUser
        
        appDelegate?.loadUsers()
        
        nPuzzle?.shuffle()
        
        /* Get Screen dimensions and center */
        let screenDim = UIScreen.main.bounds
        let myCenter = self.view.center
        
        /* set the width we care about to the lesser of screen height or width */
        var myWidth = screenDim.width <= screenDim.height ? screenDim.width : screenDim.height
        myWidth = myWidth - myWidth.truncatingRemainder(dividingBy: 3) // ensure equal tile sizes
        
        /* Create puzzle area */
        let outerWidth = myWidth - 18
        let subviewFrame = CGRect(x: 0, y: 0, width: outerWidth, height: outerWidth)
        subview = UIView(frame: subviewFrame)
        subview.center = myCenter
        
        /* Create shadowed apearence inside puzzle area */
        let innerWidth = myWidth - 30
        let shaderViewFrame = CGRect(x: 0, y:0, width: innerWidth, height: innerWidth)
        let shaderView = UIView(frame: shaderViewFrame)
        shaderView.center = myCenter
        shaderView.backgroundColor = UIColor.black
        shaderView.alpha = 0.3
        
        /* Create the tiles inside the puzzle area */
        let buttonSize = innerWidth / 3 - 2
        for i in 0...2 {
            for j in 0...2 {
                let button = MyTileButton(frame: CGRect(x: myCenter.x - (innerWidth / 2) + (CGFloat(j) * (buttonSize + 3)),
                                                        y: myCenter.y - (innerWidth / 2) + (CGFloat(i) * (buttonSize + 3)),
                                                        width: buttonSize, height: buttonSize))
                button.x = i
                button.y = j
                button.titleLabel!.font = UIFont(name: button.titleLabel!.font.fontName, size: buttonSize / 2)
                button.setTitle(String(describing: nPuzzle!.board[i][j]), for: .normal)
                button.layer.cornerRadius = buttonSize / 10
                button.addTarget(self, action: #selector(tileTouch), for: .touchUpInside)
                buttons[j].append(button)
                
                if nPuzzle?.board[i][j] == 0 {
                    button.isHidden = true
                }
            }
        }
        
        loadUserColors()
        
        /* Add all of the puzzle componenets to the main view */
        if let lview = self.view {
            lview.addSubview(subview)
            lview.addSubview(shaderView)
            for row in buttons {
                for button in row {
                    lview.addSubview(button)
                }
            }
        }
    }
    
    override func viewDidAppear(_ animated: Bool) {
        loadUserColors()
        if loadedUser != currentUser {
            newGame()
            loadedUser = currentUser
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    func loadUserColors() {
        self.users = self.appDelegate?.users
        self.currentUser = self.appDelegate?.currentUser
        
        subview.backgroundColor = users![currentUser!]!.getBoardColor()
        for row in buttons {
            for button in row {
                button.backgroundColor = users![currentUser!]!.getTileColor()
                button.setTitleColor(users![currentUser!]!.getTextColor(), for: .normal)
            }
        }
    }

    
    func tileTouch(_ sender: MyTileButton) {
        let blankX = nPuzzle!.blank[0]
        let blankY = nPuzzle!.blank[1]
        var madeMove = false
        
        if blankX == sender.x-1 && blankY == sender.y {
            nPuzzle!.makeMove(move: "U")
            madeMove = true
        }
        else if blankX == sender.x+1 && blankY == sender.y {
            nPuzzle!.makeMove(move: "D")
            madeMove = true
        }
        else if blankX == sender.x && blankY == sender.y-1 {
            nPuzzle!.makeMove(move: "L")
            madeMove = true
        }
        else if blankX == sender.x && blankY == sender.y+1 {
            nPuzzle!.makeMove(move: "R")
            madeMove = true
        }
        
        if madeMove {
            sender.setTitle(String(describing: nPuzzle!.board[sender.x][sender.y]), for: .normal)
            sender.isHidden = true
            let oldBlank = buttons[blankY][blankX]
            oldBlank.setTitle(String(describing: nPuzzle!.board[blankX][blankY]), for: .normal)
            oldBlank.isHidden = false
        }
        
        if nPuzzle!.isWon() {
            victoryLabel.isHidden = false
            for row in buttons {
                for button in row {
                    button.isUserInteractionEnabled = false
                }
            }
        }
    }

}

class MyTileButton : UIButton{
    var x : Int = 0
    var y : Int = 0
}
