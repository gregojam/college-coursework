//  James Gregory
//  gregojam
//
//  GameViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 11/9/17.
//  Last Modified by James Gregory on 12/11/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit
import SpriteKit
import GameplayKit

class GameViewController: UIViewController {
    
    let aDel = UIApplication.shared.delegate as! AppDelegate
    
    var gameScene: GameScene!

    override func viewDidLoad() {
        super.viewDidLoad()
        
        aDel.audioPlay("donut")

        
        if let view = self.view as! SKView? {
            // Load the SKScene from 'GameScene.sks'
            if let scene = GameScene(fileNamed: "GameScene") {
                // Set the scale mode to scale to fit the window
                scene.scaleMode = .aspectFill
                
                // Set reference to self in scene
                scene.viewController = self
                
                self.gameScene = scene
                
                // Present the scene
                view.presentScene(scene)
            }
            
            view.ignoresSiblingOrder = true
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        self.navigationController?.navigationBar.isHidden = true
        
        if aDel.selectedLevel != nil {
            self.gameScene.loadLevel(levelName: aDel.selectedLevel!)
            aDel.selectedLevel = nil
        }
        
        var p : SKSpriteNode!
        var selected : Int!
        for i in 0 ... 1{
            p = gameScene.player[i]
            selected = aDel.user!.playerColors![i]
            switch selected {
            case 0 :
                p.texture = SKTexture(imageNamed: "playerIconRed")
                break
                
            case 1 :
                p.texture = SKTexture(imageNamed: "playerIconGreen")
                break
                
            case 2 :
                p.texture = SKTexture(imageNamed: "playerIconBlack")
                break
                
            case 3 :
                p.texture = SKTexture(imageNamed: "playerIconYellow")
                break
                
            case 4:
                p.texture = SKTexture(imageNamed: "playerIconMagenta")
                break
            default :
                break
            }
        }
    }
    
    override var shouldAutorotate: Bool {
        return false
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Release any cached data, images, etc that aren't in use.
    }

    override var prefersStatusBarHidden: Bool {
        return true
    }
    
    func completedLevel(_ level: String) {
        aDel.user.levelsComplete?.insert(level)
    }
}
