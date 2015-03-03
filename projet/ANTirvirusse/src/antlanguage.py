from antcommand import *
from abc import ABCMeta, abstractmethod


class Expression(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def rawValue(self):
        pass

class Int(Expression):

   	def __init__(self, value):
   		self.value = value

	def rawValue(self):
		return "%i" %(self.value)

class Var(Expression):
	# '?x', where x is a variable (if x is undefined, the behavior is undefined)

	def __init__(self, var_label):
   		self.var_label = var_label

	def rawValue(self):
		return "?%s" %(self.var_label)

class Add(Expression):

	def __init__(self, left, right):
   		self.left = left
   		self.right = right

	def rawValue(self):
		return "(add %s %s)" %(self.left.rawValue(), self.right.rawValue())

class Mul(Expression):

	def __init__(self, left, right):
   		self.left = left
   		self.right = right

	def rawValue(self):
		return "(mul %s %s)" %(self.left.rawValue(), self.right.rawValue())

class Div(Expression):

	def __init__(self, left, right):
   		self.left = left
   		self.right = right

	def rawValue(self):
		return "(div %s %s)" %(self.left.rawValue(), self.right.rawValue())

class Sub(Expression):

	def __init__(self, left, right):
   		self.left = left
   		self.right = right

	def rawValue(self):
		return "(sub %s %s)" %(self.left.rawValue(), self.right.rawValue())


class See():
	# The primitive 'see' describes the cell in front of the ant: 
	# '0' is a rock, '1' is water, '2' is grass and '3' is food.

	def rawValue(self):
		return "see"

class SeeAnt():
	# The primitive 'ant_see' tells if there is an ant in front of the player's ant: 
	# '0' means no, 
	# '1' means 'yes, and it is a controlled ant', 
	# '2' means 'yes, and it is a zombie ant', and 
	# '3' means 'yes, and it is a dead ant'.

	def rawValue(self):
		return "ant_see"


class Instruction(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def rawValue(self):
        pass

class InstructionCommand(Instruction):

   	def __init__(self, command):
   		self.command = command

	def rawValue(self):
		return self.command.rawValue()

class Store(Instruction):
	# 'store!x!e' stores the evaluation of the expression 'e' in variable 'x'

	def __init__(self, var_label, expression):
		self.var_label = var_label
		self.expression = expression

	def rawValue(self):
		return "store!%s!%s" %(self.var_label, self.expression.rawValue())

class Jump(Instruction):
	# 'jump!L' to go to a label L

	def __init__(self, label):
		self.label = label

	def rawValue(self):
		return "jump!%s" %(self.label)

class ConditionalJump(Instruction):
	# 'jumpifz!x!L' to go to a label L if the variable x is 0

	def __init__(self, var_label, label):
		self.var_label = var_label
		self.label = label

	def rawValue(self):
		return "jumpifz!%s!%s" %(self.var_label, self.label)

class Fork(Instruction):
	# 'fork' to fork the current ant's code into a dead ant

	def rawValue(self):
		return "fork"


print Add(Int(1), Sub(See(), Mul(Int(3), Div(SeeAnt(), Int(5))))).rawValue()
print InstructionCommand(Left()).rawValue()

