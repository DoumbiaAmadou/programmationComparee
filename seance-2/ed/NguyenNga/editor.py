#!/usr/bin/env python
# -*- coding: utf-8 -*-

import string 

class Editor():
  """docstring for Editor"""
  def __init__(self, text):
    super(Editor, self).__init__(text)
    self.text = text
  # insere le texte a la ligne m du fichier
  def insert(self, m, subtext):
    n = len(subtext)
    for i in range(n-1):
      self.text[m+n-1+i] = self.text[m+i]
      self.text[m+i] = subtext[i]

  # Supprime les lignes de m a n
  def delete(self, m, n):
        # remplace un paragraphe de m-eme jusqu'a la fin de text a la ligne n+1
    while (n<len(self.text)):
      self.text[m]= self.text[n]
      n +=1
      m +=1
        # supprime le reste a partir de m
    while (text[m]): del text[m]
  # Remplace par subtext les lignes m a n du fichier source
  def remove(self, m, n, subtext):
    self.delete(m,n)
    self.insert(m,subtext)
    
