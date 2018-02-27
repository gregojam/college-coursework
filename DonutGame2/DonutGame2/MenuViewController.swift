//  James Gregory
//  gregojam
//
//  MenuViewController.swift
//  DonutGame2
//
//  Created by James Gregory on 11/28/17.
//  Last Modified by James Gregory on 12/11/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit
import CoreData

class MenuViewController: UIViewController {
    
    let aDel = UIApplication.shared.delegate as! AppDelegate
    var user : NSEntityDescription!
        
    let colorBtnSize = 30
    let gap = 50
    
    var warningLbl : UILabel!
    
    var p1Btns : [MyButton]!
    var p2Btns : [MyButton]!

    override func viewDidLoad() {
        super.viewDidLoad()

        // Do any additional setup after loading the view.
        
        let vc = aDel.persistentContainer.viewContext
        self.user = NSEntityDescription.entity(forEntityName: "User", in: vc)
        
        let screenSz = UIScreen.main.bounds
        let screenW = screenSz.width
        
        var lbl = UILabel(frame: CGRect(x: 0, y: screenSz.midY - 160, width: screenW, height: 50))
        lbl.text = "James' Hat Color"
        lbl.adjustsFontSizeToFitWidth = true
        lbl.textAlignment = .center
        self.view.addSubview(lbl)
        
        lbl = UILabel(frame: CGRect(x: 0, y: screenSz.midY - 60, width: screenW, height: 50))
        lbl.text = "Grace's Hat Color"
        lbl.adjustsFontSizeToFitWidth = true
        lbl.textAlignment = .center
        self.view.addSubview(lbl)
        
        let colors = aDel.playerColors
        var btn : MyButton!
        p1Btns = []
        p2Btns = []
        let spacer = (screenW / 2) - CGFloat(gap * 2)
        for i in 0...4 {
            btn = MyButton(frame: CGRect(x: 0, y: 0, width: colorBtnSize, height: colorBtnSize))
            btn.center = CGPoint(x: spacer + CGFloat(gap * i), y: screenSz.midY - 100)
            btn.layer.cornerRadius = CGFloat(colorBtnSize / 2)
            btn.layer.borderWidth = 2
            btn.layer.backgroundColor = colors[i].cgColor
            btn.layer.borderColor = colors[i].cgColor
            btn.nDex = i
            btn.addTarget(self, action: #selector(colorChoose1), for: .touchUpInside)
            p1Btns.append(btn)
            self.view.addSubview(btn)
            
            btn = MyButton(frame: CGRect(x: 0, y: 0, width: colorBtnSize, height: colorBtnSize))
            btn.center = CGPoint(x: spacer + CGFloat(gap * i), y: screenSz.midY)
            btn.layer.cornerRadius = CGFloat(colorBtnSize / 2)
            btn.layer.borderWidth = 2
            btn.layer.backgroundColor = colors[i].cgColor
            btn.layer.borderColor = colors[i].cgColor
            btn.nDex = i
            btn.addTarget(self, action: #selector(colorChoose2), for: .touchUpInside)
            p2Btns.append(btn)
            self.view.addSubview(btn)
        }
        
        warningLbl = UILabel(frame: CGRect(x: 0, y: screenSz.midY + 70, width: screenW, height: 30))
        warningLbl.text = "Pressing reset button again will reset all game data!"
        warningLbl.textColor = UIColor.red
        warningLbl.adjustsFontSizeToFitWidth = true
        warningLbl.textAlignment = .center
        warningLbl.isHidden = true
        self.view.addSubview(warningLbl)
        
        let resetBtn = UIButton(frame: CGRect(x: 0, y: 0,
                                              width: 100, height: 30))
        resetBtn.center = CGPoint(x: screenSz.midX, y: screenSz.midY + 120)
        resetBtn.layer.cornerRadius = CGFloat(10)
        resetBtn.layer.backgroundColor = UIColor.gray.cgColor
        resetBtn.setTitle("Reset Game", for: .normal)
        resetBtn.addTarget(self, action: #selector(userReset), for: .touchUpInside)
        self.view.addSubview(resetBtn)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        let p1 = aDel.user.playerColors![0]
        let p2 = aDel.user.playerColors![1]
        p1Btns[p1].layer.borderColor = UIColor.lightGray.cgColor
        p1Btns[p2].isHidden = true
        
        p2Btns[p2].layer.borderColor = UIColor.lightGray.cgColor
        p2Btns[p1].isHidden = true
        
        warningLbl.isHidden = true
    }
    
    override func viewWillDisappear(_ animated: Bool) {
        aDel.saveContext()
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    func colorChoose1(_ sender : MyButton) {
        for i in 0 ... 4{
            if i == sender.nDex {
                p1Btns[i].layer.borderColor = UIColor.gray.cgColor
                p2Btns[aDel.user.playerColors![0]].isHidden = false
                p2Btns[i].isHidden = true
                aDel.user.playerColors![0] = sender.nDex
            }
            else {
                p1Btns[i].layer.borderColor = p1Btns[i].layer.backgroundColor
            }
        }
    }
    
    func colorChoose2(_ sender : MyButton) {
        for i in 0 ... 4{
            if i == sender.nDex {
                p2Btns[i].layer.borderColor = UIColor.gray.cgColor
                p1Btns[aDel.user.playerColors![1]].isHidden = false
                p1Btns[i].isHidden = true
                aDel.user.playerColors![1] = sender.nDex
            }
            else {
                p2Btns[i].layer.borderColor = p2Btns[i].layer.backgroundColor
            }
        }
    }
    
    func userReset(_ sender : UIButton) {
        if warningLbl.isHidden {
            warningLbl.isHidden = false
        }
        else {
            warningLbl.isHidden = true
            colorChoose1(p1Btns[0])
            colorChoose2(p2Btns[1])
            
            aDel.selectedLevel = "Launch_Screen"
            
            aDel.user.levelsComplete = []
            aDel.saveContext()
            aDel.user.soundsRecorded = false
            
            navigationController?.popToRootViewController(animated: true)
        }
    }
    
    class MyButton : UIButton {
        var nDex : Int!
    }

}
