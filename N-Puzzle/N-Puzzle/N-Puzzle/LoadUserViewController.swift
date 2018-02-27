//
//  LoadUserViewController.swift
//  N-Puzzle
//
//  Created by Curly Burns on 10/12/17.
//  Copyright © 2017 Gregory, James M. All rights reserved.
//

import UIKit

class LoadUserViewController: UIViewController {
    
    var appDelegate = UIApplication.shared.delegate as? AppDelegate
    
    @IBOutlet var userNameField : UITextField!
    @IBOutlet var passwordField : UITextField!
    @IBOutlet var warningLabel : UILabel!
    
    @IBAction func cancelButton(_ sender: UIButton) {
        navigationController?.popViewController(animated: true)
    }
    
    @IBAction func loadUser() {
        let name = userNameField.text!
        let password = passwordField.text!
        
        let user = appDelegate?.users[name]
        if user != nil && user!.getPassword() == password{
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
    

    /*
    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        // Get the new view controller using segue.destinationViewController.
        // Pass the selected object to the new view controller.
    }
    */

}
