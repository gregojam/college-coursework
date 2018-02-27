//  James Gregory
//  gregojam
//
//  GameScene.swift
//  DonutGame2
//
//  Created by James Gregory on 11/9/17.
//  Last Modified by James Gregory on 12/11/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import SpriteKit
import GameplayKit

class GameScene: SKScene {
    
    var viewController : GameViewController!
    
    // "Screen" is set up as a 30 x 32 grid
    let screenHLim : CGFloat = 362.5
    let screenWLim : CGFloat = 387.5
    
    // Important variables for gameplay
    var player : [SKSpriteNode]!
    var P : Int!
    var keysHeld : Int!
    var moving : String!
    var lastMove : CFTimeInterval?
    var buttonFree : Bool!
    
    // Important level Information
    var currLevel : String?
    var portalPairs : [PortalPair]!
    var switches : [[CGPoint]]!
    var onSwitch : [Bool]!

    
    override func didMove(to view: SKView) {
        self.player = [SKSpriteNode(), SKSpriteNode()]
        self.P = 0
        self.moving = ""
        self.buttonFree = true
        self.portalPairs = []
        self.switches = []
        self.onSwitch = [false, false]

        
        // Draw left/right border
        for i in 0 ..< 32 {
            let leftWall = SKSpriteNode(imageNamed: "wallIcon")
            let rightWall = SKSpriteNode(imageNamed: "wallIcon")
            addObj(obj: leftWall, x: -1, y: i)
            addObj(obj: rightWall, x: 32, y: i)
        }
    }
    
    // Add a given Node to the Scene at the given grid position
    func addObj(obj : SKSpriteNode, x : Int, y : Int) {
        self.addChild(obj)
        obj.position = convertPos(x: x, y: y)
    }
    
    // Toggle sliding walls for switch at player position
    func toggleWalls(p: CGPoint) {
        for s in switches {
            if(s[0] == player[P].position){
                for i in 1 ..< s.count {
                    let thereNow = self.atPoint(s[i])
                    if thereNow.name == "wall" {
                        thereNow.removeFromParent()
                    }
                    else if thereNow.name != "player" && s[i] != p {
                        let obj = SKSpriteNode(imageNamed: "wallIcon")
                        self.addChild(obj)
                        obj.name = "wall"
                        obj.position = s[i]
                    }
                }
                return
            }
        }
    }
    
    // Convert grid coordinates to screen position
    func convertPos(x: Int, y: Int) -> CGPoint{
        let x1 = Double(x)
        let y1 = Double(y)
        
        // Convert grid coordinates to screen coordinates for first point
        let tmpX  = x1 > 15 ? 12.5 + 25 * (x1 - 16) : -12.5 - 25 * (15 - x1)
        let tmpY = y1 > 14 ? -12.5 - 25 * (y1 - 15) : 12.5 + 25 * (14 - y1)
        return CGPoint(x: tmpX, y: tmpY)
    }
    
    // Load a given level into the Scene's "screen" area
    func loadLevel(levelName : String) {
        // Remove old level
        for node in self.children {
            // Don't remove controls!!!
            if(-screenWLim ... screenWLim ~= node.position.x) {
                node.removeFromParent()
            }
        }
        
        // Open level .txt file
        let path = Bundle.main.bundlePath + "/" + levelName
        do {
            let data = try String(contentsOfFile: path)
            var lines = data.components(separatedBy: .newlines)
            
            self.currLevel = levelName
            
            // Reset level variables
            self.P = 0
            self.keysHeld = 0
            self.portalPairs = []
            self.switches = []
            self.onSwitch = [false, false]
            self.moving = ""
            self.lastMove = nil
            self.buttonFree = true
            
            // Read in Portal Pairs
            let n = Int(lines[0])!
            for i in 0 ..< n {
                self.portalPairs.append(PortalPair(input: lines[i+1]))
            }
            
            // Read in Switch lists
            let m = Int(lines[n + 1])!
            for i in 0 ..< m {
                let parts = lines[n+2+i].components(separatedBy: .whitespaces)
                
                switches.append([])
                var j = 0
                while j < parts.count {
                    switches[i].append(convertPos(x: Int(parts[j])!, y: Int(parts[j+1])!))
                    j = j + 2
                }
            }
            
            // Remove visual border and read in the level map
            let base = n+m+3
            for i in base ... base+29 {
                lines[i].characters.removeFirst()
                let end = lines[i].index(lines[i].startIndex, offsetBy: 32)
                lines[i] = lines[i].substring(to: end)
 
                var j = 0
                for char in lines[i].characters {
                    switch char {
                    case "@":   // Player
                        let obj = SKSpriteNode(imageNamed: "playerIconRed")
                        obj.alpha = 0.8
                        obj.name = "player"
                        addObj(obj : obj, x : j, y : i-base)
                        obj.zPosition = 1
                        self.player[0] = obj
                        break
                    case "&":   // Player2
                        let obj = SKSpriteNode(imageNamed: "playerIconGreen")
                        obj.alpha = 0.8
                        obj.name = "player"
                        addObj(obj : obj, x : j, y : i-base)
                        obj.zPosition = 1
                        self.player[1] = obj
                        break
                    case "x":   // Wall
                        let obj = SKSpriteNode(imageNamed: "wallIcon")
                        obj.name = "wall"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    case "o":   // Donut
                        let obj = SKSpriteNode(imageNamed: "donutIcon")
                        obj.name = "donut"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    case "n":   // Door
                        let obj = SKSpriteNode(imageNamed: "doorIcon")
                        obj.name = "door"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    case "k":   // Key
                        let obj = SKSpriteNode(imageNamed: "keyIcon")
                        obj.name = "key"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    case "p":   // Portal
                        let obj = SKSpriteNode(imageNamed: "portalIcon")
                        obj.name = "portal"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    case "s":   // Switch
                        let obj = SKSpriteNode(imageNamed: "switchIcon")
                        obj.name = "switch"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    case "w":   // Sliding Wall
                        let obj = SKSpriteNode(imageNamed: "wallIcon")
                        obj.name = "wall"
                        addObj(obj : obj, x : j, y : i-base)
                        break
                    default:
                        break
                    }
                    j += 1
                }
            }
        }catch _ {
            print("Error loading level")
        }
        
        self.isPaused = false
    }
    
    // Find which direction is being pressed
    func readDPad(tPos : CGPoint){
        if -30 ... 30 ~= tPos.x && 30 ... 90 ~= tPos.y {
            moving = "U"
        }
        else if -30 ... 30 ~= tPos.x && -90 ... -30 ~= tPos.y {
            moving = "D"
        }
        else if -90 ... -30 ~= tPos.x && -30 ... 30 ~= tPos.y {
            moving = "L"
        }
        else if 30 ... 90 ~= tPos.x && -30 ... 30 ~= tPos.y {
            moving = "R"
        }
        else{
            moving = ""
        }
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        // Get reference to the dPad
        let dPad = self.childNode(withName: "//dPad") as! SKSpriteNode
        
        // Get reference to the button
        let button = self.childNode(withName: "//button") as! SKSpriteNode
        
        // Get reference to the menu button
        let menuBtn = self.childNode(withName: "//menuBtn") as! SKSpriteNode
        
        for t in touches {
            // Iff touch is on the dPad, handle it accordingly
            var tPos = t.location(in: dPad)
            if -90 ... 90 ~= tPos.x && -90 ... 90 ~= tPos.y{
                readDPad(tPos : tPos)
            }
            
            // Iff touch is on the button, handle it
            tPos = t.location(in: button)
            if sqrt(tPos.x * tPos.x + tPos.y * tPos.y) <= 60 {
                P = (P + 1) % 2
                buttonFree = false
                
            }
            
            // Iff touch is on the menu button, handle it
            tPos = t.location(in: menuBtn)
            if -35 ... 35 ~= tPos.x && -10 ... 10 ~= tPos.y {
                self.viewController.performSegue(withIdentifier: "menuSegue", sender: self.viewController)
            }
        }
    }
    
    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
        // Get reference to the dPad
        let dPad = self.childNode(withName: "//dPad") as! SKSpriteNode
        
        // Get reference to the button
        let button = self.childNode(withName: "//button") as! SKSpriteNode
        
        // Get reference to the menu button
        let menuBtn = self.childNode(withName: "//menuBtn") as! SKSpriteNode
        
        var offDPad = true
        var offButton = true
        for t in touches {
            // Iff touch is on the dPad, handle it accordingly
            var tPos = t.location(in: dPad)
            if -90 ... 90 ~= tPos.x && -90 ... 90 ~= tPos.y {
                offDPad = false
                readDPad(tPos : tPos)
            }
            if(offDPad) {
                moving = ""
                lastMove = nil
            }
            
            // Iff touch in on the button, handle it
            tPos = t.location(in: button)
            if(sqrt(tPos.x * tPos.x + tPos.y * tPos.y) <= 60){
                offButton = false
                if(buttonFree){
                    P = (P + 1) % 2
                    buttonFree = false
                }
            }
            
            // Iff touch is on the menu button, handle it
            tPos = t.location(in: menuBtn)
            if -35 ... 35 ~= tPos.x && -10 ... 10 ~= tPos.y {
                self.viewController.performSegue(withIdentifier: "menuSegue", sender: self.viewController)
            }
        }
        if(offButton){
            buttonFree = true
        }
    }
    
    // Reset moving variables when touches end
    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        moving = ""
        lastMove = nil
        buttonFree = true
    }
    
    // Same as touchesEnded()
    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        moving = ""
        lastMove = nil
        buttonFree = true
    }
    
    /*  This is where all movement/collision is handled.
        Player movement is limited to occur only once every eighth of a second to simulate "retro" movement.
     
        Though the game is played in landscape mode, the direction moved is labeled as though the device is in
        portrait mode. (i.e. "Move Up" means the player is moving towards the earpiece if played on a phone)
     
        Iff the player movement would collide with a wall or a door (with no key held) the movement is not allowed.
        Iff the player collides with a key or the donut, the latter is removed from the scene.
        Iff the player collides with a portal, the player is teleported to the portal paired with it.
    */
    override func update(_ currentTime: TimeInterval) {
        
        if(lastMove == nil || (CACurrentMediaTime() - lastMove! >= 0.125)){
            var stepTo = player[P] as SKNode
            var newPos = player[P].position
            switch moving {
            case "U":   // Move Up
                stepTo = self.atPoint(CGPoint(x: player[P].position.x, y: player[P].position.y + 25))
                newPos.y = min(player[P].position.y + 25, screenHLim)
                break
            case "D":   // Move Down
                stepTo = self.atPoint(CGPoint(x: player[P].position.x, y: player[P].position.y - 25))
                newPos.y = max(player[P].position.y - 25, -screenHLim)
                break
            case "L":   // Move Left
                stepTo = self.atPoint(CGPoint(x: player[P].position.x - 25, y: player[P].position.y))
                newPos.x = max(player[P].position.x - 25, -screenWLim)
                break
            case "R":   // Move Right
                stepTo = self.atPoint(CGPoint(x: player[P].position.x + 25, y: player[P].position.y))
                newPos.x = min(player[P].position.x + 25, screenWLim)
                break
            default:
                // Not moving, skip collision detection
                return
            }
            if(stepTo.name != "wall" && stepTo.name != "player") {
                if(stepTo.name != "door" || keysHeld > 0) {
                    if(stepTo.name == "door") {
                        viewController.aDel.audioPlay("door")
                        stepTo.removeFromParent()
                        keysHeld = keysHeld - 1
                    }
                    if(stepTo.name == "key") {
                        viewController.aDel.audioPlay("key")
                        stepTo.removeFromParent()
                        keysHeld = keysHeld + 1
                    }
                    
                    if(onSwitch[P]){
                        onSwitch[P] = false
                        toggleWalls(p: newPos)
                    }
                    
                    player[P].position = newPos
                    
                    if(stepTo.name == "switch") {
                        viewController.aDel.audioPlay("switch")
                        onSwitch[P] = true
                        toggleWalls(p: newPos)
                    }
                    
                    if(stepTo.name == "portal") {
                        var telPos : CGPoint? = player[P].position
                        for pair in portalPairs {
                            telPos = pair.teleport(from: player[P].position)
                            if telPos != nil {
                                if telPos != player[(P+1)%2].position {
                                    viewController.aDel.audioPlay("portal")

                                    let telTo = self.atPoint(telPos!)
                                    player[P].position = telPos!
                                    
                                    if(telTo.name == "key") {
                                        viewController.aDel.audioPlay("key")
                                        telTo.removeFromParent()
                                        keysHeld = keysHeld + 1
                                    }
                                    
                                    if(telTo.name == "switch") {
                                        viewController.aDel.audioPlay("switch")
                                        onSwitch[P] = true
                                        toggleWalls(p: telPos!)
                                    }
                                }
                                break
                            }
                        }
                    }
                    
                    if(stepTo.name == "donut") {
                        viewController.aDel.audioPlay("donut")
                        stepTo.removeFromParent()
                        
                        self.isPaused = true
                        
                        viewController.aDel.user.levelsComplete!.insert(currLevel!)
                        viewController.aDel.saveContext()
                        
                        let count = viewController.aDel.user.levelsComplete!.count
                        if count % 2 == 0 {
                            viewController.aDel.newStory = (count / 2) - 1
                        }
                        else {
                            viewController.aDel.newStory = -1
                        }
                        
                        self.viewController.performSegue(withIdentifier: "menuSegue", sender: self.viewController)
                    }
                }
            }
            lastMove = CACurrentMediaTime()
        }
    }
}

// Subclass used for storing portal pairs
class PortalPair {
    var first : CGPoint!
    var second : CGPoint!
    
    // Converts the lines from the level blueprint into a new Portal Pair
    init(input : String){
        let parts = input.components(separatedBy: .whitespaces)
        
        let x1 = Double(parts[0])!
        let y1 = Double(parts[1])!
        let x2 = Double(parts[2])!
        let y2 = Double(parts[3])!
        
        // Convert grid coordinates to screen coordinates for first point
        var tmpX  = x1 > 15 ? 12.5 + 25 * (x1 - 16) : -12.5 - 25 * (15 - x1)
        var tmpY = y1 > 14 ? -12.5 - 25 * (y1 - 15) : 12.5 + 25 * (14 - y1)
        self.first = CGPoint(x: tmpX, y: tmpY)
        
        // Convert grid coordinates to screen coordinates for second point
        tmpX  = x2 > 15 ? 12.5 + 25 * (x2 - 16) : -12.5 - 25 * (15 - x2)
        tmpY = y2 > 14 ? -12.5 - 25 * (y2 - 15) : 12.5 + 25 * (14 - y2)
        self.second = CGPoint(x: tmpX, y: tmpY)
    }
    
    // Iff the given point is one of the points in the pairing, return the opposite point, Iff not return nil.
    func teleport(from: CGPoint) -> CGPoint? {
        if(from == self.first){
            return self.second
        }
        if(from == self.second) {
            return self.first
        }
        return nil
    }
}
