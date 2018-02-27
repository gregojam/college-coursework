//
//  NewUserViewController.swift
//  N-Puzzle
//
//  Created by Curly Burns on 10/11/17.
//  Copyright © 2017 Gregory, James M. All rights reserved.
//

import UIKit

class NewUserViewController: UIViewController {
    
    var appDelegate = UIApplication.shared.delegate as? AppDelegate
    
    @IBOutlet var userNameField : UITextField!
    @IBOutlet var passwordField : UITextField!
    @IBOutlet var warningLabel : UILabel!
    
    @IBAction func cancelButton(_ sender: UIButton) {
        navigationController?.popViewController(animated: true)
    }
    
    @IBAction func createUser(_ sender: UIButton) {
        let name = userNameField.text!
        let password = passwordField.text!
        if appDelegate?.users[name] == nil{
            appDelegate?.addUser(name: name, password: password)
            appDelegate?.currentUser = name
            navigationController?.popViewController(animated: true)
        }
        else {
            warningLabel.isHidden = false
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()

    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

}
