//  James Gregory
//  gregojam
//
//  AppDelegate.swift
//  DonutGame2
//
//  Created by James Gregory on 11/9/17.
//  Last Modified by James Gregory on 11/28/17.
//  Copyright © 2017 James Gregory. All rights reserved.
//

import UIKit
import CoreData
import AVFoundation

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

    var window: UIWindow?
    
    var selectedLevel : String?
    
    var newStory : Int!
    
    let playerColors : [UIColor] = [.red, .green, .black, .yellow, .magenta]
    
    var user : UserModel!
    
    var iAudioPlayer : AVAudioPlayer?
    
    var soundPaths : [String : URL]!
    
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
        // Override point for customization after application launch.
        
        self.selectedLevel = "Launch_Screen"
        
        self.newStory = -1
        
        self.soundPaths = [:]
                
        let viewContext = persistentContainer.viewContext
        
        let fetchUser = NSFetchRequest<NSFetchRequestResult>(entityName: "User")
        do {
            let fetchRes = try viewContext.fetch(fetchUser) as! [UserModel]
            if fetchRes.count == 0 {
                user = NSEntityDescription.insertNewObject(forEntityName: "User", into: viewContext) as! UserModel
                user.levelsComplete = []
                user.playerColors = [0, 1]
                user.soundsRecorded = false
                
                do {
                    try viewContext.save()
                }catch {
                    print("Error saving preferences")
                }
            }
            else {
                user = fetchRes[0]
            }
        }catch {
            print("Problem loading User")
        }
        
        return true
    }

    func applicationWillResignActive(_ application: UIApplication) {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
    }

    func applicationDidEnterBackground(_ application: UIApplication) {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    }

    func applicationWillEnterForeground(_ application: UIApplication) {
        // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
    }

    func applicationDidBecomeActive(_ application: UIApplication) {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    }

    func applicationWillTerminate(_ application: UIApplication) {
        // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    }
    
    lazy var persistentContainer: NSPersistentContainer = {
        /*
         The persistent container for the application. This implementation
         creates and returns a container, having loaded the store for the
         application to it. This property is optional since there are legitimate
         error conditions that could cause the creation of the store to fail.
         */
        let container = NSPersistentContainer(name: "Model")
        container.loadPersistentStores(completionHandler: { (storeDescription, error) in
            if let error = error as NSError? {
                // Replace this implementation with code to handle the error appropriately.
                // fatalError() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development.
                
                /*
                 Typical reasons for an error here include:
                 * The parent directory does not exist, cannot be created, or disallows writing.
                 * The persistent store is not accessible, due to permissions or data protection when the device is locked.
                 * The device is out of space.
                 * The store could not be migrated to the current model version.
                 Check the error message to determine what the actual problem was.
                 */
                fatalError("Unresolved error \(error), \(error.userInfo)")
            }
        })
        return container
    }()
    
    // MARK: - Core Data Saving support
    
    func saveContext () {
        let context = persistentContainer.viewContext
        if context.hasChanges {
            do {
                try context.save()
            } catch {
                // Replace this implementation with code to handle the error appropriately.
                // fatalError() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development.
                let nserror = error as NSError
                fatalError("Unresolved error \(nserror), \(nserror.userInfo)")
            }
        }
    }

    // MARK: - Audio
    
    // ----------------------------------------------------------------------
    func audioRecordingPath(_ title : String) -> URL{
        if soundPaths[title] != nil {
            return soundPaths[title]!
        }
        
        let fileManager = FileManager()
        
        let documentsFolderUrl = try? fileManager.url(for: .documentDirectory,
                                                      in: .userDomainMask,
                                                      appropriateFor: nil,
                                                      create: false)
        
        var path : URL!
        switch title {
        case "key" :
            path = documentsFolderUrl!.appendingPathComponent("KeySound.m4a")
            break
        case "door" :
            path = documentsFolderUrl!.appendingPathComponent("DoorSound.m4a")
            break
        case "switch" :
            path = documentsFolderUrl!.appendingPathComponent("SwitchSound.m4a")
            break
        case "portal" :
            path = documentsFolderUrl!.appendingPathComponent("KeySound.m4a")
            break
        case "donut" :
            path = documentsFolderUrl!.appendingPathComponent("DonutSound.m4a")
            break
        default :
            path = documentsFolderUrl!.appendingPathComponent("DefaultSound.m4a")
        }
        
        soundPaths[title] = path
        return path
    }
    
    func audioPlay(_ title : String) {
        var settingPlayerError: NSError?

        let audioRecordingURL = audioRecordingPath(title)
        
        do{
            iAudioPlayer = try AVAudioPlayer(contentsOf: audioRecordingURL)

        } catch let error1 as NSError {
            settingPlayerError = error1
            iAudioPlayer = nil
            NSLog("AudioTab: error in setting audio playback: \(String(describing: settingPlayerError))")
        }

        if let player = iAudioPlayer{
            player.rate = 2
            player.enableRate = true
            player.prepareToPlay()
            player.play()
        }
    }
    
    // ----------------------------------------------------------------------
    // MARK: AVAudioPlayerDelegate protocol
    // ----------------------------------------------------------------------
    
    
    // ----------------------------------------------------------------------
    //  delegate method for Responding to Sound Playback Completion
    //  audioPlayerDidFinishPlaying(_:successfully:)
    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer,
                                     successfully flag: Bool){
        
        if flag{
            NSLog("AudioTab: Audio player stopped correctly")
        } else {
            NSLog("AudioTab: Audio player did not stop correctly")
        }
        
        iAudioPlayer = nil
    }
    
    // ----------------------------------------------------------------------
    func audioPlayerDecodeErrorDidOccur(_ player: AVAudioPlayer, error: Error?) {
        // we should properly respond to any error that may occur during playback
        NSLog("AudioTab: audioPlayerDecodeErrorDidOccur !!!")
    }
    
    
    // ----------------------------------------------------------------------
    // deprecated as of iOS 8.0
    // ----------------------------------------------------------------------
    func audioPlayerBeginInterruption(_ player: AVAudioPlayer) {
        /* The audio session is deactivated here */
    }
    
    // ----------------------------------------------------------------------
    // deprecated as of iOS 8.0
    // ----------------------------------------------------------------------
    func audioPlayerEndInterruption(_ player: AVAudioPlayer,
                                    withOptions flags: Int) {
        if flags == AVAudioSessionInterruptionFlags_ShouldResume{
            player.play()
        }
    }

}

