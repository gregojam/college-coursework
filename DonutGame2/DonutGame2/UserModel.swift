//
//  James Gregory
//  gregojam
//
//  UserModel.swift
//  DonutGame2
//
//  Created by James Gregory on 12/6/17.
//  Last Modified by James Gregory on 12/15/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import Foundation
import CoreData

class UserModel : NSManagedObject {
    @NSManaged var levelsComplete : Set<String>?
    @NSManaged var playerColors : [Int]?
    @NSManaged var soundsRecorded : Bool
}
