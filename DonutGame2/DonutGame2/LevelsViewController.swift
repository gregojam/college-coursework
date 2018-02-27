//  James Gregory
//  gregojam
//
//  LevelsViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 11/28/17.
//  Last Modified by James Gregory on 12/11/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit

class LevelsViewController: UITableViewController {
        
    let levels : [String] = [
        "Level_1",
        "Level_2",
        "Level_3",
        "Level_4",
        "Level_5",
        "Level_6",
        "Level_7",
        "Level_8",
        "Level_9",
        "Level_10",
        "Level_11",
        "Level_12"
    ]
    
    let aDel = UIApplication.shared.delegate as! AppDelegate

    override func viewDidLoad() {
        super.viewDidLoad()
        
        tableView.tableFooterView = UIView()

        // Do any additional setup after loading the view.
    }
    
    override func viewWillAppear(_ animated: Bool) {
        self.tableView.reloadData()
        
        let vcs = navigationController?.viewControllers
        vcs?[(vcs?.count)! - 2].navigationItem.title = nil
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return levels.count
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "myCell", for: indexPath) as! MyCell
        
        cell.title.text = levels[indexPath.row]
        
        if aDel.user.levelsComplete!.contains(levels[indexPath.row]) {
            cell.icon.isHidden = false
            switch indexPath.row % 3 {
            case 0 :
                cell.backgroundColor = UIColor.cyan
                break
            case 1 :
                cell.backgroundColor = UIColor.yellow
                break
            case 2 :
                cell.backgroundColor = UIColor.magenta
                break
            default :
                break
            }
        }
        else {
            cell.icon.isHidden = true
            cell.backgroundColor = UIColor.white
        }

        return cell
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let cell = tableView.cellForRow(at: indexPath)! as! MyCell
        
        if aDel.selectedLevel != nil && cell.title.text == aDel.selectedLevel {
            aDel.selectedLevel = nil
            tableView.deselectRow(at: indexPath, animated: true)
            var vcs = navigationController?.viewControllers
            vcs?[(vcs?.count)! - 2].navigationItem.title = nil
        }
        else {
            aDel.selectedLevel = cell.title.text
            var vcs = navigationController?.viewControllers
            vcs?[(vcs?.count)! - 2].navigationItem.title = "Load " + cell.title.text!
        }
    }

}

class MyCell : UITableViewCell {
    @IBOutlet var title : UILabel!
    @IBOutlet var icon : UIImageView!
}
