// N-Puzzle
// James Gregory    gregojam
// "Programming Assignment 105"
// "A290/A590 / Fall 2017"
// 10-9-17

// OptionsViewController.swift

import UIKit

class OptionsViewController: UITableViewController {
    
    var appDelegate = UIApplication.shared.delegate as? AppDelegate
    var users : [String: UserModel]?
    var currentUser : String?
    
    let rowsInSection : [Int] = [1, 1, 1, 2]
    let titlesForSection : [String] = ["Tile Color", "Board Color", "Text Color", "User Profile"]
    let colorConv : [String : UIColor] = ["Blue" : UIColor.blue,
                                          "Green" : UIColor.green,
                                          "Red" : UIColor.red,
                                          "Yellow" : UIColor.yellow,
                                          "Purple" : UIColor.purple]
    
    @IBAction func colorButtonisPressed(_ sender: MyOptionsButton) {
        self.users = self.appDelegate?.users
        self.currentUser = self.appDelegate?.currentUser
        
        /* Highlight selected button, and unhighlight others in cell */
        for i in 0...4 {
            let cell = sender.superview?.superview as! MyTableViewCell
            let button = cell.buttons[i]
            if button == sender {
                button.backgroundColor = colorConv[button.currentTitle!]
            }
            else {
                button.backgroundColor = UIColor.clear
            }
        }
        
        /* Change and save user color settings */
        switch sender.section {
        case "Tile Color" :
            users![currentUser!]!.setTileColor(color: colorConv[sender.currentTitle!]!)
            break
        case "Board Color" :
            users![currentUser!]!.setBoardColor(color: colorConv[sender.currentTitle!]!)
            break
        case "Text Color" :
            users![currentUser!]!.setTextColor(color: colorConv[sender.currentTitle!]!)
            break
        default :
            break
        }
        appDelegate?.saveUsers()
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        /* Remove extra, empty cells */
        tableView.tableFooterView = UIView()
    }
    
    override func viewDidAppear(_ animated: Bool) {
        self.loadView()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 4
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return section < rowsInSection.count ? rowsInSection[section] : 0
    }
    
    override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String {
        return section < titlesForSection.count ? titlesForSection[section] : ""
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        self.users = self.appDelegate?.users
        self.currentUser = self.appDelegate?.currentUser
        
        var cell : MyTableViewCell = MyTableViewCell()
        if indexPath.section <= 2 {
            cell = tableView.dequeueReusableCell(withIdentifier: "ColorCell", for: indexPath) as! MyTableViewCell
            for button in cell.buttons{
                button.layer.borderWidth = 2
                button.layer.borderColor = colorConv[button.currentTitle!]?.cgColor
                button.layer.cornerRadius = 5
                button.section = titlesForSection[indexPath.section]
                
                var selectedColor : UIColor
                switch button.section {
                case "Tile Color" :
                    selectedColor = users![currentUser!]!.getTileColor()
                    break
                case "Board Color" :
                    selectedColor = users![currentUser!]!.getBoardColor()
                    break
                case "Text Color" :
                    selectedColor = users![currentUser!]!.getTextColor()
                    break
                default :
                    selectedColor = UIColor.clear
                }
                
                if colorConv[button.currentTitle!] == selectedColor {
                    button.backgroundColor = selectedColor
                }
            }
        }
        else if indexPath.section == 3 {
            if indexPath.row == 0{
                cell = tableView.dequeueReusableCell(withIdentifier: "NewUserCell", for: indexPath) as! MyTableViewCell
            }
            else {
            cell = tableView.dequeueReusableCell(withIdentifier: "LoadUserCell", for: indexPath) as! MyTableViewCell
            }
        }
        
        return cell
    }
    
    override func tableView(_ tableView: UITableView, willSelectRowAt indexPath: IndexPath) -> IndexPath? {
        return nil
    }

    override func tableView(_ tableView: UITableView, shouldHighlightRowAt indexPath: IndexPath) -> Bool {
        return false
    }
    


}

class MyTableViewCell : UITableViewCell {
    @IBOutlet var buttons : [MyOptionsButton]!
}

class MyOptionsButton : UIButton {
    var section : String = ""
}
