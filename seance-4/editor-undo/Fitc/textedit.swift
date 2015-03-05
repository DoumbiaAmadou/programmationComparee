//
//  main.swift
//  TextEdit
//
//  Created by Vladislav Fitc on 28/02/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

import Foundation

typealias Text = String
typealias Chariot = Int

let standardInput = NSFileHandle.fileHandleWithStandardInput()

var textsHead: [(Text, Chariot)] = [("", 0)]
var textsTail: [(Text, Chariot)] = []

var currentState: (Text, Chariot) {
    return textsHead.last ?? ("",0)
}

var currentText: Text {
    return textsHead.last?.0 ?? ""
}

var currentChariot: Chariot {
    return textsHead.last?.1 ?? 0
}

var currentTextLength: Int {
    return countElements(currentText.0)
}

var currentChariotIndex: String.Index {
    return advance(currentText.0.startIndex, currentState.1)
}

enum Command {
    case AddString(String)
    case MoveChariot(Int)
    case DeleteChar
    case Undo
    case Redo
}

func processCommand(command: Command){
    switch command {
        
    case .AddString(let string):
        let left = currentText.substringToIndex(currentChariotIndex)
        let right = currentText.substringFromIndex(currentChariotIndex)
        textsHead.append(left + string + right, currentChariot+countElements(string))
        
    case .MoveChariot(let offset):
        switch currentChariot + offset {
        case let c where c > currentTextLength:
            textsHead.append(currentText, currentTextLength)
            
        case let c where c < 0:
            textsHead.append(currentText, 0)
            
        default:
            textsHead.append(currentText, currentChariot + offset)
        }
        
    case .DeleteChar:
        let left = currentText.substringToIndex(currentChariotIndex.predecessor())
        let right = currentText.substringFromIndex(currentChariotIndex)
        
        textsHead.append(left + right, currentChariot-1)
        
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
        return Command.AddString(input.substringFromIndex(advance(input.startIndex, 2)))
        
    case "M" where countElements(input) > 2:
        if let offset = input.substringFromIndex(advance(input.startIndex, 2)).toInt() {
            return Command.MoveChariot(offset)
        } else {
            fallthrough
        }
    
    case "D":
        return Command.DeleteChar
        
    case "U":
        return Command.Undo
        
    case "R":
        return Command.Redo
        
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



