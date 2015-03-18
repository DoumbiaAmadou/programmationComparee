//
//  main.swift
//  TextEdit
//
//  Created by Vladislav Fitc on 28/02/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

import Foundation

typealias Text = String
typealias Focus = Int

let standardInput = NSFileHandle.fileHandleWithStandardInput()

var textsHead: [(Text, Focus)] = [("", 0)]
var textsTail: [(Text, Focus)] = []

var currentState: (Text, Focus) {
    return textsHead.last ?? ("",0)
}

var currentText: Text {
    return textsHead.last?.0 ?? ""
}

var currentFocus: Focus {
    return textsHead.last?.1 ?? 0
}

var currentTextLength: Int {
    return countElements(currentText.0)
}

var currentFocusIndex: String.Index {
    return advance(currentText.0.startIndex, currentState.1)
}

enum Command {
    case InsertString(String)
    case MoveFocus(Int)
    case DeleteChar
    case InsertChar(Character)
    case ChangeChar(Character)
    case Undo
    case Redo
}

func processCommand(command: Command){
    switch command {
        
    case .InsertString(let string):
        let left = currentText.substringToIndex(currentFocusIndex)
        let right = currentText.substringFromIndex(currentFocusIndex)
        textsHead.append(left + string + right, currentFocus+countElements(string))
        
    case .MoveFocus(let offset):
        switch currentFocus + offset {
        case let c where c > currentTextLength:
            textsHead.append(currentText, currentTextLength)
            
        case let c where c < 0:
            textsHead.append(currentText, 0)
            
        default:
            textsHead.append(currentText, currentFocus + offset)
        }
        
    case .DeleteChar:
        let left = currentText.substringToIndex(currentFocusIndex.predecessor())
        let right = currentText.substringFromIndex(currentFocusIndex)
        
        textsHead.append(left + right, currentFocus-1)
        
    case .InsertChar(let char):
        let left = currentText.substringToIndex(currentFocusIndex)
        let right = currentText.substringFromIndex(currentFocusIndex)
        
        textsHead.append(left + String(char) + right, currentFocus)
        
    case .ChangeChar(let char):
        let left = currentText.substringToIndex(currentFocusIndex.predecessor())
        let right = currentText.substringFromIndex(currentFocusIndex)

        textsHead.append(left + String(char) + right, currentFocus)
        
    case .Undo:
        if let previousText = textsHead.last {
            textsTail.append(previousText)
            textsHead.removeLast()
        }
        
    case .Redo:
        if let nextText = textsTail.last {
            textsHead.append(nextText)
            textsTail.removeLast()
        }
    }
    
}

func processInput(input: String) -> Command? {
    if countElements(input) < 1 {
        println("Empty string")
        return .None
    }
    
    let inputCmd = input.substringToIndex(advance(input.startIndex, 1))
    
    switch inputCmd {
        
    case "A" where countElements(input) > 2:
        return Command.InsertString(input.substringFromIndex(advance(input.startIndex, 2)))
        
    case "C" where countElements(input) > 2:
        let char = Array(input.substringFromIndex(advance(input.startIndex, 2)))[0]
        return Command.ChangeChar(char)
        
    case "D":
        return Command.DeleteChar
        
    case "I" where countElements(input) > 2:
        let char = Array(input.substringFromIndex(advance(input.startIndex, 2)))[0]
        return Command.InsertChar(char)
        
    case "M" where countElements(input) > 2:
        if let offset = input.substringFromIndex(advance(input.startIndex, 2)).toInt() {
            return Command.MoveFocus(offset)
        } else {
            fallthrough
        }

    case "R":
        return Command.Redo
        
    case "U":
        return Command.Undo
        
    default:
        println("Wrong command: \(input)")
        return .None
    }
}

while true {
    if let input = NSString(data: standardInput.availableData, encoding: NSUTF8StringEncoding) {
        if let command = processInput(input.stringByTrimmingCharactersInSet(NSCharacterSet(charactersInString: "\n"))) {
            processCommand(command)
        }
        
        println("Current text: \(currentText)")
    }
}



