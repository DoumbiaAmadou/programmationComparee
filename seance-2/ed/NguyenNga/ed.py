#!/usr/bin/env python
# -*- coding: utf-8 -*-

import string 

from editor import Editor

# lire le fichier
f = open('simpleText.txt', 'r')
mainText = []
for row in f.readlines():
    mainText.append(row)

f.close()

 # lire instruction 
def read(self, instruction):
  instruction = instruction.split(",")
  return instruction[0]

 # interpreter instruction
def interpret(self, instruction):
  instruction = instruction.split(",")
  if (instruction[0] == 'I'): # insere un texte
    self.insert(instruction[1])
              
  elif (instruction[0] == 'D'): # Suprime les lignes 
    self.delete(instruction[1], instruction[2])
              
  elif (instruction[0] == 'R'): # Remplace les lignes
    self.remove(instruction[1], instruction[2])

    
        
