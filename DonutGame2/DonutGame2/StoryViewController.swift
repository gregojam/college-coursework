//  James Gregory
//  gregojam
//
//  StoryViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 12/15/17.
//  Last Modified by James Gregory on 12/15/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit

class StoryViewController: UIViewController {
    
    let aDel = UIApplication.shared.delegate as! AppDelegate
    
    let story = [
    "Legend has it, there was once a brave hero who set out to collect the greatest " +
    "dozen donuts the world had ever known, to give to the student of a class in which " +
    "he served as a valiant undergraduate instructor. The hero travelled far and wide, " +
    "overcoming the greatest challenges the angry Donut God could muster. In the end, the " +
    "hero delivered the donuts to his class, and the story of his triumph was passed on for " +
    "minutes!\n\n Two lovers, James and Grace, happened upon the old legend months after the " +
    "glory had faded away, and have become completely entranced with the tale. They decide " +
    "to gather up 5 differently colored hats and their magically linked backpaks, and head " +
    "off to try to relive the legend on their own. Who knows what kind of devious devices " +
    "the vengeful Donut God has devised since the days of old!",
    
    "As James and Grace collect their fourth donut, their hearts fill with pride. They're " +
    "doing it. They're really doing it! However, they also know that they have only bested " +
    "4 of the Donut God's attempts to keep them from the delicious treasures they so desire.\n\n" +
    "\t\"How are you feeling? Still pumped abou these donuts?\", James asked Grace.\n\n\t\"Hell " +
    "yeah! We can beat anything that smelly old Donut God can throw at us! WE'RE UNSTOPPABLE!!!\", " +
    "Grace excitedly replied.\n\nAnd with that, they gathered their hats and backpacks, and headed " +
    "towards the next donut.",
    
    "6 donuts! They are half-way legendary! They can almost taste the victory, and it is almost as " +
    "sweet as the warm round, iced gems they are gathering. Unfortunately, the Donut God was " +
    "becomming nervous. He had already been bested by another mortal so, so long ago, and he was " +
    "just recently able to begin showing his face at the social gatherings of the other Food Gods. " +
    "His life was finally starting to return to the unquestioned glory that it once was. He could " +
    "NOT allow this to happen again! He was going to have to pull out the big guns! He was going " +
    "to have to...\n\n CONTINUE BUILDING THE SAME SORT OF PUZZLES AROUND HIS PRECIOUS DONUTS!\n\n " +
    "\t\"Good luck, Mortals! You're going to need it!\", the Donut God bellowed with a jelly-filled, " +
    "sprinkle-covered grin.",
        
    "James and Grace now have 8 donuts.",
    
    "They were close. They were so close to becomming the stuff of legends. They were getting tired, " +
    "they were covered in sugar and crumbs, they were sticky and thirsty, but most of all... they " +
    "were determined. They had dreamt about the taste of these donuts, and the glory that would come " +
    "from collecting them. They would eat their fill, and be remembered, no... revered for an amount " +
    "of time after they returned from their conquest. They would be filled with the energy of a " +
    "thousand Suns, and then pass out from a sugar-crash the likes of which they could hardly even " +
    "imagine. There was no time to waste! Magic backpacks, and trusty, differently colored hats in " +
    "tow, Our heroes ventured off to collect the last 2 donuts to complete their dozen.",
    
    "They did it! They completed the qeust of quests! They have become legends " +
    "amongst legends! They have... fulfilled their destinies... of... destinies.\n\nThe Donut God " +
    "hangs his donutty head in shame, fists clenched so tight that his cream filling begins to spill " +
    "out. Again, he was the laughing-stock of the food gods. He would not stand for this. He would " +
    "have his revenge.\n\nGrace and James cared not about the Donut Gods threats. They simply " +
    "slapped him on his doughy cheeks, and told \"Better luck next time, Champ!\"\n\n As our heroes " +
    "began their journey back home, the took each other by the hand, shared a quick kiss, and began " +
    "discussing what they would have for lunch.\n\n\t\tTHE END"
    ]
    
    
    @IBOutlet var segmentBar : UISegmentedControl!
    @IBOutlet var label : UILabel!
    
    @IBAction func loadChapter(_ sender : UISegmentedControl) {
        let i = sender.selectedSegmentIndex
        
        if (aDel.user.levelsComplete?.count)! / 2 >= i + 1 {
            label.text = story[i]
        }
        else {
            label.text = "Play the game! For every 2 levels you complete, " +
                            "you'll unlock more of the story!"
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        // Do any additional setup after loading the view.
    }
    
    override func viewWillAppear(_ animated: Bool) {
        if (aDel.user.levelsComplete?.count)! < 2 {
            label.text = "Play the game! For every 2 levels you complete, " +
                            "you'll unlock more of the story!"
        }
        else{
            segmentBar.isHidden = false
            
            if aDel.newStory >= 0 {
                segmentBar.selectedSegmentIndex = aDel.newStory
                label.text = story[aDel.newStory]
                aDel.newStory = -1
            }
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    

    /*
    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        // Get the new view controller using segue.destinationViewController.
        // Pass the selected object to the new view controller.
    }
    */

}
