//
//  main.swift
//  5Des
//
//  Created by Vladislav Fitc on 05/03/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

import Foundation

for i in 0..<60000 {

    let values = [i/10000 % 6 + 1, i/1000 % 6 + 1, i/100 % 6 + 1, i/10 % 6 + 1, i % 6 + 1]
    var occurencies:[Int:Int] = [1:0, 2:0, 3:0, 4:0, 5:0, 6:0]
    
    for value in values {
        occurencies[value] = occurencies[value]! + 1
    }

    let suite = occurencies.values.array.filter{ $0 == 0 }.isEmpty
    let full = !occurencies.values.array.filter{ $0 == 3 }.isEmpty && !occurencies.values.array.filter{ $0 == 2 }.isEmpty
    
    if (full || suite) {
        for value in values {
            print("\(value) ")
        }
        print("\n")
    }
}