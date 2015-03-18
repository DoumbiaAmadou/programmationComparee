 # -*- coding: utf-8 -*-

from abc import ABCMeta, abstractmethod

### Expressions of ant language 

class Expression(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def rawValue(self):
        pass

class Int(Expression):
	# Integer variable
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

# Arithmetics

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

### Instructions of ant language 

class Instruction(object):
    __metaclass__ = ABCMeta

    def __init__(self, instr_label=None):
   		self.instr_label = instr_label

    def rawValue(self):
		if self.instr_label is not None:
			return self.instr_label+">"
		else:
			return ""

class InstructionCommand(Instruction):

   	def __init__(self, command, instr_label=None):
   		self.command = command
   		super(InstructionCommand, self).__init__(instr_label)

	def rawValue(self):
		return super(InstructionCommand, self).rawValue() + self.command.rawValue()

class Store(Instruction):
	# 'store!x!e' stores the evaluation of the expression 'e' in variable 'x'

	def __init__(self, var_label, expression, instr_label=None):
		self.var_label = var_label
		self.expression = expression
		super(Store, self).__init__(instr_label)

	def rawValue(self):
		return super(Store, self).rawValue() + "store!%s!%s" %(self.var_label, self.expression.rawValue())

class Jump(Instruction):
	# 'jump!L' to go to a label L

	def __init__(self, to_label, instr_label=None):
		self.to_label = to_label
		super(Jump, self).__init__(instr_label)

	def rawValue(self):
		return super(Jump, self).rawValue() +"jump!%s" %(self.to_label)

class ConditionalJump(Instruction):
	# 'jumpifz!x!L' to go to a label L if the variable x is 0

	def __init__(self, var_label, to_label, instr_label=None):
		self.var_label 	= var_label
		self.to_label 	= to_label
		super(ConditionalJump, self).__init__(instr_label)

	def rawValue(self):
		return super(ConditionalJump, self).rawValue() + "jumpifz!%s!%s" %(self.var_label, self.to_label)

class Fork(Instruction):
	# 'fork' to fork the current ant's code into a dead ant

	def __init__(self, instr_label=None):
		super(Fork, self).__init__(instr_label)

	def rawValue(self):
		return super(Fork, self).rawValue() + "fork"


# Tests

# from antcommand import Left
def test():	
	print Add(Int(1), Sub(See(), Mul(Int(3), Div(SeeAnt(), Var("x"))))).rawValue()

	print InstructionCommand(Left()).rawValue()
	print InstructionCommand(Left(), "label").rawValue()

	print Store("x", Add(Int(1), Int(2))).rawValue()
	print Store("x", Add(Int(1), Int(2)), "label").rawValue()

	print Jump("jlabel").rawValue()
	print Jump("jlabel", "label").rawValue()

	print ConditionalJump("x", "jlabel").rawValue()
	print ConditionalJump("x", "jlabel", "label").rawValue()

	print Fork().rawValue()
	print Fork("label").rawValue()