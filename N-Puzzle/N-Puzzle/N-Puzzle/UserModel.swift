// N-Puzzle
// James Gregory    gregojam
// "Programming Assignment 105"
// "A290/A590 / Fall 2017"
// 10-9-17

// UserModel.swift

import Foundation
import UIKit

class UserModel : NSObject, NSCoding {
    /* Archiving Paths */
    static let DocumentsDir = FileManager().urls(for: .documentDirectory,
                                                 in: .userDomainMask).first!
    static let ArchiveURL = DocumentsDir.appendingPathComponent(".users")
    /* Arching Paths END */
    
    private let name : String
    private let password : String
    
    private var tileColor : UIColor
    private var boardColor : UIColor
    private var textColor : UIColor
    
    init(name: String, password: String) {
        self.name = name
        self.password = password
        tileColor = UIColor.green
        boardColor = UIColor.red
        textColor = UIColor.blue
    }
    
    func getName() -> String {
        return self.name
    }
    
    func getPassword() -> String {
        return self.password
    }
    
    func setTileColor(color : UIColor) {
        tileColor = color
    }
    
    func getTileColor() -> UIColor {
        return tileColor
    }
    
    func setBoardColor(color : UIColor) {
        boardColor = color
    }
    
    func getBoardColor() -> UIColor {
        return boardColor
    }
    
    func setTextColor(color : UIColor) {
        textColor = color
    }
    
    func getTextColor() -> UIColor {
        return textColor
    }
    
    /* NSCoding */
    func encode(with coder: NSCoder) {
        coder.encode(name, forKey: "name")
        coder.encode(password, forKey: "password")
        coder.encode(tileColor, forKey: "tileColor")
        coder.encode(boardColor, forKey: "boardColor")
        coder.encode(textColor, forKey: "textColor")
    }
    
    required init?(coder decoder: NSCoder) {
        name = decoder.decodeObject(forKey: "name") as! String
        password = decoder.decodeObject(forKey: "password") as! String
        tileColor = decoder.decodeObject(forKey: "tileColor") as! UIColor
        boardColor = decoder.decodeObject(forKey: "boardColor") as! UIColor
        textColor = decoder.decodeObject(forKey: "textColor") as! UIColor
    }
    /* NSCoding END */
}
