 # -*- coding: utf-8 -*-

from abc import ABCMeta, abstractmethod
from antlanguage import Int, Add, Jump, Store

'''
  Une commande "command" attachee à un fourmi avec l'identificateur "ant_id"
'''

class AttachedCommand(object):
  def __init__(self, ant_id, command):
    self.ant_id = ant_id
    self.command = command

  def rawValue(self):
    return str(self.ant_id)+":"+self.command.rawValue()


class Command(object):
  __metaclass__ = ABCMeta

  @abstractmethod
  def rawValue(self):
    pass

class Left(Command):

  def rawValue(self):
    return "left"

class Right(Command):

  def rawValue(self):
    return "right"

class Forward(Command):

	def rawValue(self):
		return "forward"

class Attack(Command):
   	
  def __init__(self, level):
    self.level = level

  def rawValue(self):
    return "attack@%i" %(self.level)


class Hack(Command):
  #commands est une liste d'objets Instruction
  def __init__(self, instructions):
    self.instructions = instructions

  def rawValue(self):
    instructions_list = ""
    for instruction in self.instructions:
      instructions_list += instruction.rawValue()+";"
    return "hack@[%s]" %(instructions_list)


# Tests

def test():
  c = Left()
  print c.rawValue()

  c = Right()
  print c.rawValue()

  c = Forward()
  print c.rawValue()

  c = Attack(10)
  print c.rawValue()

  commands = [Store("x", Add(al.Int(1), Int(2)), "label"), Jump("label")]
  c = Hack(commands)
  print c.rawValue()
