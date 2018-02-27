//  James Gregory
//  gregojam
//
//  HelpViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 11/28/17.
//  Last Modified by James Gregory on 12/11/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit

class HelpViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()

        let screenSz = UIScreen.main.bounds
        
        let navBarHeight = navigationController?.navigationBar.frame.height
        let tabBarHeight = tabBarController?.tabBar.frame.height
        let imageHeight = screenSz.height - navBarHeight! - tabBarHeight!
        
        let instructions = UIImageView(image : #imageLiteral(resourceName: "instructionsImage"))
        instructions.frame = CGRect(x: 0, y: navBarHeight!,
                                  width: screenSz.width, height: imageHeight)
        self.view.addSubview(instructions)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

}
