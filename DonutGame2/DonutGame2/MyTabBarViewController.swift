//  James Gregory
//  gregojam
//
//  MyTabBarViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 11/28/17.
//  Last Modified by James Gregory on 11/28/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit

class MyTabBarViewController: UITabBarController {
    
    let aDel = UIApplication.shared.delegate as! AppDelegate

    override func viewDidLoad() {
        super.viewDidLoad()
    
        // Do any additional setup after loading the view.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        
        self.navigationController?.navigationBar.isHidden = false
        UIViewController.attemptRotationToDeviceOrientation()
        
        if aDel.newStory >= 0 {
            self.selectedIndex = 3
        }
    }
    
    override var shouldAutorotate: Bool {
        return true
    }
}
