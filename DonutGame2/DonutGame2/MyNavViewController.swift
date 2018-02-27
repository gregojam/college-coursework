//  James Gregory
//  gregojam
//
//  MyNavViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 11/28/17.
//  Last Modified by James Gregory on 11/28/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit

class MyNavViewController: UINavigationController {

    override func viewDidLoad() {
        super.viewDidLoad()

        self.interactivePopGestureRecognizer?.isEnabled = false
        // Do any additional setup after loading the view.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    override var shouldAutorotate: Bool {
        return self.visibleViewController!.shouldAutorotate
    }
    
    override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return .landscape
    }

}
