from abc import ABCMeta, abstractmethod


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

  def __init__(self, commands):
    self.commands = commands

  def rawValue(self):
    return "hack@[%s]" %(self.commands)


# c = Left()
# print c.rawValue()

# c = Attack(10)
# print c.rawValue()

# c = Hack("list of commands")
# print c.rawValue()