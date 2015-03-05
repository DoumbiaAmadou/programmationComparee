//
//  main.swift
//  5Des
//
//  Created by Vladislav Fitc on 05/03/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

import Foundation

enum Dice: Int {
    typealias Carry = Bool
    case One    = 1
    case Two    = 2
    case Three  = 3
    case Four   = 4
    case Five   = 5
    case Six    = 6
    
    init(){
        self = One
    }
    
    func succ() -> (Dice, Carry) {
        switch self {
        case .Six:
            return (.One, true)
        default:
            return (Dice(rawValue: self.rawValue+1)!, false)
        }
    }
}

var dices = [Dice(), Dice(), Dice(), Dice(), Dice()]

func succ(dices: [Dice]) -> [Dice] {
    var succ = [Dice]()
    
    for (var i = dices.count-1; i >= 0; --i){
        succ.append(dices[i].succ().0)
        if !dices[i].succ().1 {
            for (var j = i-1; j >= 0; --j){
                succ.append(dices[j])
            }
            break
        }
    }
    
    return succ.reverse()
}

func showDices(dices: [Dice]){
    println(dices.map{"\($0.rawValue) "}.reduce("", combine: +))
}

while dices != [.Six, .Six, .Six, .Six, .Six] {
    dices = succ(dices)
    var occurencies:[Dice:Int] = [.One:0, .Two:0, .Three:0, .Four:0, .Five:0, .Six:0]
    for dice in dices {
        occurencies[dice] = occurencies[dice]! + 1
    }
    let suite = occurencies.values.array.filter{ $0 == 0 }.isEmpty
    let full = !occurencies.values.array.filter{ $0 == 3 }.isEmpty && !occurencies.values.array.filter{ $0 == 2 }.isEmpty

    if (full || suite) {
        showDices(dices)
    }
    
}