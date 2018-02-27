//
//  James Gregory
//  gregojam
//
//  AudioViewController.swift
//  DonutGame2
//
//  Created by Mitja Hmeljak on 2016-04-18.
//  Last Modified by James Gregory on 12/15/17.
//  Copyright © 2016 B481 Spring 2016. All rights reserved.
//

import UIKit
import AVFoundation

class AudioViewController: UIViewController,
    
    // The delegate of an AVAudioPlayer object must adopt the AVAudioPlayerDelegate protocol.
    // All of the methods in this protocol are optional.
    // They allow a delegate to respond to audio interruptions and audio decoding errors,
    // and to the completion of a sound’s playback.
    AVAudioPlayerDelegate,  // we're going to playback audio
    
    
    // The delegate of an AVAudioRecorder object must adopt the AVAudioRecorderDelegate protocol.
    // All of the methods in this protocol are optional.
    // They allow a delegate to respond to audio interruptions and audio decoding errors,
    // and to the completion of a recording.
    
    AVAudioRecorderDelegate // we're going to record audio
    
{
    // ----------------------------------------------------------------------
    // instance variables:
    
    let aDel = UIApplication.shared.delegate as! AppDelegate
    
    var iAudioRecorder: AVAudioRecorder?
    
    var autoStop : DispatchWorkItem?
    
    @IBOutlet var leftBtn : UIButton?
    @IBOutlet var yesBtn : UIButton?
    @IBOutlet var noBtn : UIButton?
    @IBOutlet var label : UILabel?
    @IBOutlet var goLabel : UILabel?
    
    var objTitles : [String] = ["key", "door", "switch", "portal", "donut"]
    var curr : Int? {
        didSet {
            if curr != nil && curr! < objTitles.count {
                yesBtn?.isHidden = true
                noBtn?.isHidden = true
                goLabel?.isHidden = true
                
                let title = objTitles[curr!]
                self.label?.text = "What sound does a " + title + " make?"
                
                if ((leftBtn?.target(forAction: #selector(myPlayButton), withSender: self)) != nil) {
                    leftBtn?.removeTarget(self, action: #selector(myPlayButton), for: .touchUpInside)
                }
                leftBtn?.addTarget(self, action: #selector(myRecordButton), for: .touchUpInside)
                leftBtn?.setTitle("Record", for: .normal)
            }
            else {
                aDel.user.soundsRecorded = true
                aDel.saveContext()
                self.performSegue(withIdentifier: "gameSegue", sender: self)
            }
        }
    }
    
    // ----------------------------------------------------------------------
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        
        self.navigationController?.navigationBar.isHidden = true
        
        if aDel.user.soundsRecorded {
            self.performSegue(withIdentifier: "gameSegue", sender: self)
        }
        curr = 0
        noBtn?.isHidden = true
    }
    
    // ----------------------------------------------------------------------
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    @IBAction func noButton(_ sender: UIButton) {
        let tmp = curr
        curr = tmp
    }
    
    @IBAction func yesButton(_ sender: UIButton) {
        curr! += 1
    }
    
    
    // ----------------------------------------------------------------------
    func myRecordButton(_ sender: AnyObject) {
        
        /* Ask for permission to see if we can record audio */
        
        var error: NSError?
        let session = AVAudioSession.sharedInstance()
        
        do {
            try session.setCategory(
                AVAudioSessionCategoryRecord,
                with: .duckOthers)
            
            do {
                try session.setActive(true)
                NSLog("AudioTab: Successfully activated the audio session")
                
                leftBtn?.isEnabled = false
                
                session.requestRecordPermission{[weak self](allowed: Bool) in
                    
                    if allowed{
                        self!.startRecordingAudio()
                    } else {
                        NSLog("AudioTab: We don't have permission to record audio");
                        self!.curr = self!.objTitles.count+1
                    }
                    
                }
            } catch _ {
                NSLog("AudioTab: Could not activate the audio session")
            }
            
        } catch let error1 as NSError {
            error = error1
            
            if let theError = error {
                NSLog("AudioTab: An error occurred in setting the audio " +
                    "session category. Error = \(theError)")
            }
            
        }
    }
    
    func myStopRecordingButton(_ sender: UIButton) {
        if(self.iAudioRecorder != nil){
            self.iAudioRecorder!.stop()
            label?.text = "Does recording sound like a " + objTitles[curr!] + "?"
            
            leftBtn?.removeTarget(self, action: #selector(myRecordButton), for: .touchUpInside)
            leftBtn?.addTarget(self, action: #selector(myPlayButton), for: .touchUpInside)
            leftBtn?.setTitle("Play", for: .normal)
            leftBtn?.isEnabled = true
            
            yesBtn?.isHidden = false
            noBtn?.isHidden = false
        }
        self.autoStop!.cancel()
    }
    
    func myPlayButton(_ sender: UIButton) {        
        aDel.audioPlay(objTitles[curr!])
    }
    
    
    // ----------------------------------------------------------------------
    func startRecordingAudio(){
        
        var settingRecorderError: NSError?
        
        let audioRecordingURL = aDel.audioRecordingPath(objTitles[curr!])
        
        do {
            iAudioRecorder = try AVAudioRecorder(url: audioRecordingURL,
                                                 settings: audioRecordingSettings() as! [String : AnyObject])
        } catch let error1 as NSError {
            settingRecorderError = error1
            iAudioRecorder = nil
            NSLog("AudioTab: error in setting audio recording: \(String(describing: settingRecorderError))")
        }
        
        if let recorder = iAudioRecorder{
            
            recorder.delegate = self
            /* Prepare the recorder and then start the recording */
            
            if recorder.prepareToRecord() && recorder.record(){
                
                NSLog("AudioTab: Successfully started to record.")
                
                goLabel!.isHidden = false
                
                /* After 5 seconds, let's stop the recording process */
                let delayInSeconds = 1.5
                let delayInNanoSeconds =
                    DispatchTime.now() + Double(Int64(delayInSeconds * Double(NSEC_PER_SEC))) / Double(NSEC_PER_SEC)
                
                let workTmp : DispatchWorkItem = DispatchWorkItem(block : {
                    if(self.iAudioRecorder != nil) {
                        self.iAudioRecorder!.stop()
                        self.label?.text = "Does recording sound like a " + self.objTitles[self.curr!] + "?"
                        
                        self.leftBtn?.removeTarget(self, action: #selector(self.myRecordButton),
                                                   for: .touchUpInside)
                        self.leftBtn?.addTarget(self, action: #selector(self.myPlayButton),
                                                for: .touchUpInside)
                        self.leftBtn?.setTitle("Play", for: .normal)
                        self.leftBtn?.isEnabled = true

                        self.yesBtn?.isHidden = false
                        self.noBtn?.isHidden = false
                    }
                })
                
                self.autoStop = workTmp
                
                DispatchQueue.main.asyncAfter(deadline: delayInNanoSeconds, execute: self.autoStop!)
                
            } else {
                NSLog("AudioTab: Failed to record.")
                iAudioRecorder = nil
            }
            
        } else {
            NSLog("AudioTab: Failed to create an instance of the audio recorder")
        }
        
    }
    
    
    // ----------------------------------------------------------------------
    func audioRecordingSettings() -> NSDictionary {
        
        /* Let's prepare the audio recorder options in the dictionary.
         Later we will use this dictionary to instantiate an audio
         recorder of type AVAudioRecorder */
        
        return [
            AVFormatIDKey : Int(kAudioFormatMPEG4AAC),
            AVSampleRateKey : 16000.0,
            AVNumberOfChannelsKey : NSNumber(value: 1 as Float),
            AVEncoderAudioQualityKey : AVAudioQuality.high.rawValue
        ]
        
    }
    
    
    // ----------------------------------------------------------------------
    // MARK: AVAudioRecorderDelegate protocol
    // ----------------------------------------------------------------------
    
    
    
    // ----------------------------------------------------------------------
    // Called by the system when a recording is stopped
    //   or has finished due to reaching its time limit.
    func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder,
                                         successfully flag: Bool){
        
        if flag {
            NSLog("AudioTab: Successfully stopped the audio recording process")
        } else {
            NSLog("AudioTab: Stopping the audio recording failed")
        }
        
        /* Here we don't need the audio recorder anymore */
        self.iAudioRecorder = nil;
        goLabel?.isHidden = true
    } // end of audioRecorderDidFinishRecording()
    
}


