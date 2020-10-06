#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 17 23:15:50 2020

@author: Fred Valdez Ameneyro

My Evolutionary Algorithm for Decision Trees
Features:
Evolution seeded with the output of existing methods
Evolve towards different objectives
Can output multiple trees with particular advantages when there are conflicting objectives
Existing solutions can be evolved further
test
"""
import numpy as np
import random as rd
import operator as op
import math
from inspect import signature
import pandas as pd
import copy

def get_arity(operator):
	"""
	Returns the arity of the method, operator or funtion as an int
	:param operator: is a method, operator or funtion
	"""
	sig = signature(operator)
	arity = len(sig.parameters)
	return arity
	
def add_crucial_value(attribute,value):
	"""
	crucial values are the relevant values that are used to split the data in the trees
	returns: the index in the attribute of the newly added value
	"""
	if value in attribute.crucial_values:
		index = attribute.crucial_values.index(value)
		return index
	else:
		index = len(attribute.crucial_values)
		attribute.crucial_values = attribute.crucial_values + [value]
		print("Added crucial value ", str(value), " to attribute ", attribute.name, ". All values:", str(attribute.crucial_values))
		return index
		
class Objective:
	def __init__(self,
		to_max = True,
		best_known_value = None,
		worst_known_value = None):
		self.index = index
		self.to_max = to_max
		self.best_known_value = best_known_value
		self.worst_known_value = worst_known_value
	
	def set_best_known_value(value):
		self.best_known_value = value
	
	def set_worst_known_value(value):
		self.worst_known_value = value
		
class Individual:
	def __init__(
			self,
			generation_of_creation,
			genotype, 
			phenotype = None,
			objective_values = None,
			n_dominators = None,                 #used in MOEA, pareto dominance
			n_dominated_solutions = None,        #used in MOEA, pareto dominance
			dominated_solutions = None,          #used in NSGA-II
			local_crowding_distance = None,      #used in NSGA-II
			non_domination_rank = None):          #used in NSGA-II

		self.generation_of_creation = generation_of_creation
		self.genotype = genotype
		self.phenotype = phenotype
		self.n_dominators = n_dominators
		self.n_dominated_solutions = n_dominated_solutions
		self.dominated_solutions = dominated_solutions
		self.objective_values = objective_values
		self.local_crowding_distance = local_crowding_distance
		self.non_domination_rank = non_domination_rank
		
		self.n_objectives = len(objective_values)
		
class Attribute:
	def __init__(self,
			index,                        #index of the attribute in the dataset
			name,
			type = None,
			crucial_values = [],
			all_values = [],
			available_values = []):
		"""
		s
		"""
	
		self.index = index
		self.name = name
		self.type = type
		self.all_values = all_values
		self.crucial_values = crucial_values
		self.available_values = available_values
	
	def __str__(self):
		return "Attribute " + self.name + ",idx:" + str(self.index) + ",n_crucial_values:" + str(len(self.crucial_values))  
	
class DT_Node:
	def __init__(self, parent = None):
		"""
		children are sorted: the first one is the true, the second one is the false
		"""
		self.updated=False
		self.parent=parent
		self.children = []
		self.children_names = []
		self.output_label = None
		self.attribute = None
		self.operator = None
		self.comparable_value = None
		self.comparable_value_index = None
	
	def update(self,attribute,comparable_value_index,comparable_value,operator):
		self.updated = True
		self.attribute = attribute
		self.comparable_value = comparable_value #float(self.attribute.crucial_values[comparable_value_index]) #all are floats rn
		self.comparable_value_index = comparable_value_index
		self.operator = operator
	
	def add_child(self,name,child=None):
		if child is None:
			child = DT_Node(parent=self)
		self.children = self.children + [child]
		self.children_names = self.children_names + [name]
		print("Added child named ", name , ",new child count", str(len(self.children)))
		return child
	
	def evaluate(self,data_row):
		#print("Evaluating node", self)
		if self.output_label is None:
			#print("node",str(self))
			#print("data_row:\n",data_row)
			#print("data value",data_row.iloc[self.attribute.index])
			#print("attribute name",self.attribute.name)
			#print("attribute index",self.attribute.index)
			#print("operator",str(self.operator))
			#print("comparable_value",str(self.comparable_value))
			
			try:
				comparison = self.operator(data_row.iloc[self.attribute.index],self.comparable_value)
			except:
				print("Error in evaluation")
				print("node",str(self))
				print("data_row:\n",data_row)
				print("data value",data_row.iloc[self.attribute.index])
				print("attribute name",self.attribute.name)
				print("attribute index",self.attribute.index)
				print("operator",str(self.operator))
				print("comparable_value",str(self.comparable_value))
				return None
			
			if comparison == True:
				#print("Going to child", self.children_names[0])
				return self.children[0].evaluate(data_row)
			else:
				#print("Going to child", self.children_names[1])
				return self.children[1].evaluate(data_row)
			
		else:
			#print("Output:",self.output_label)
			return self.output_label

	def is_terminal(self):
		return self.children == []

	def is_root(self):
		return self.parent is None

	def get_subtree_nodes(self, include_self = True, include_terminals = True):
		"""
		Returns a list with all the nodes of the subtree with this node as the root node, including himself
		"""
		nodes = [self]
		i = 0
		while i < len(nodes):
			if not nodes[i].is_terminal():
				nodes.extend(nodes[i].children)
			i += 1
		if include_self:
			return nodes
		else:
			return nodes[1:]

	def get_max_depth(self, depth = 0):
		"""
		Returns the max depth of this tree as an int
		"""
		new_depth = depth + 1
		if self.is_terminal():
			return new_depth
		else:
			return max([child.my_depth(new_depth) for child in self.children])

	def node_already_in_branch(self, node=None):
		if self.parent is None:
			return False
		else:
			if self == node:
				return True
			else:
				return parent.node_already_in_branch(node=node)

	def copy(self, parent=None):
		"""
		Don't give arguments. Returns an unrelated new item with the same characteristics
		"""
		the_copy = DT_Node(parent = parent)
		the_copy.update(attribute=self.attribute,
						comparable_value_index = self.comparable_value_index,
						comparable_value = self.comparable_value,
						operator = self.operator)
		#print(self.is_terminal())
		if not self.is_terminal():
			for child_index, child in enumerate(self.children):
				copy_child = child.copy(parent=the_copy)
				the_copy.add_child(name=self.children_names[child_index], child=copy_child)
				
		return the_copy
		
	def __eq__(self,other):
		return self.attribute.name == other.attribute.name and self.operator == other.operator and self.comparable_value == other.comparable_value

	def __str__(self):
			if self.attribute is None:
				return "Node: terminal, op:" + str(self.operator) + ",value:" + str(self.comparable_value) + ",ol:" + str(self.output_label) + ",n_children:" + str(len(self.children))
			else:
				return "Node:" + self.attribute.name + ",op:" + str(self.operator) + ",value:" + str(self.comparable_value) + ",ol:" + str(self.output_label) + ",n_children:" + str(len(self.children))

class DecisionTree_EA: #oblique, binary trees
	def __init__(self):
			
		self.output_labels = []
		self.unique_output_labels = []
		self.attributes = {}
		self.operators = []
		self.objectives = {}
		self.n_objectives = 0
		self.generation = 0
		self.population = []
		self.parsing_operators = []
		self.operators_db = {"<":op.lt,">=":op.ge,"<=":op.le,">":op.gt,"==":op.eq,"!=":op.ne}
		self.inverse_operators = {op.lt:op.ge,op.le:op.gt,op.gt:op.le,op.ge:op.lt,op.eq:op.ne}
		print("Called class DecisionTree_EA")
										
	def add_operator(self,operator): #operators with compatibility to certain attributes?
		if operator not in self.operators:
			self.operators.append(operator)
			print("Added new operator:",str(operator))
	
	def add_objective(self,
			to_max = True,
			best_known_value = None,
			worst_known_value = None):
		self.objectives[self.n_objectives] = Objective(to_max = to_max,
													best_known_value = best_known_value,
													worst_known_value = worst_known_value)
		self.n_objectives = self.n_objectives + 1

	def attribute_mutation(self,
			individual):
		pass

	def attribute_value_mutation(self,
			individual):
		pass
		
	def swap_child(node1,new_parent,old_child,new_child):
		pass
	
	def one_point_crossover(self,
			individual1,
			individual2):
		print("individual1",individual1)
		print("individual1",individual2)
		copy1 = individual1.copy()
		copy2 = individual2.copy()
		print("copy1",copy1)
		print("copy2",copy2)
		nodes1 = copy1.get_subtree_nodes(include_self = False)
		nodes2 = copy2.get_subtree_nodes(include_self = False)
		print("len(nodes1)",len(nodes1))
		print("len(nodes2)",len(nodes2))

		node1 = rd.choice(nodes1)
		node2 = rd.choice(nodes2)
		print(node1)
		print(node2)
		
		nodes1 = node1.get_subtree_nodes(include_self = False)
		nodes2 = node2.get_subtree_nodes(include_self = False)
		print("len(nodes1)",len(nodes1))
		print("len(nodes2)",len(nodes2))
		
		print("parent before")
		print(node1.parent)
		print(node2.parent)
		
		temp = node1.parent
		node1.parent = node2.parent
		node2.parent = temp
		node1.parent.add_child(
		
		print("parent after")
		print(node1.parent)
		print(node2.parent)
		
		nodes1 = copy1.get_subtree_nodes(include_self = False)
		nodes2 = copy2.get_subtree_nodes(include_self = False)
		print("len(nodes1)",len(nodes1))
		print("len(nodes2)",len(nodes2))
		
		return copy1, copy2

		
	def evaluate_tree(self,root_node,provisional_data=None):
		pass
		
		if provisional_data is None:
			data = self.data
		else:
			data = provisional_data
		output_array = []
		corrects = 0
		for index, row in data.iterrows():
			#if index in [i for i in range(13)]:
			#print("in row index", index)
			#print(row)
			output = root_node.evaluate(row)
			if output == self.output_labels[index]:
				corrects += 1
		return corrects/len(data.index)
			
	def adapt_to_data(self, labels, data):
		self.data = pd.DataFrame(data)
		for i, attribute in enumerate(self.data.columns):
			self.attributes[attribute] = Attribute(index=i, name=attribute)
			print("added attribute",attribute)
		self.output_labels = list(labels)
		self.unique_output_labels = set(list(labels))
		self.dataset = data
		print("unique_output_labels",self.unique_output_labels)
	
	def parse_tree_r(self, tree):
		pd_tree = pd.DataFrame(tree)
		labels = pd_tree["RHS"]
		rules = pd_tree["LHS"]
		#rules = rules[:20]                                   #for testing
		root_node = DT_Node()
		for rule_index,rule in enumerate(rules):
			print("Parsing rule ", rule)
			node = root_node
			rule = str(rule)
			comparisons = rule.split(" & ")           #& symbol should not be in the name of any attribute, verify later
			n_comparisons = len(comparisons)-1
			for comp_idx,comparison in enumerate(comparisons):
				if comparison in node.children_names:
					child_index = node.children_names.index(comparison)
					child = node.children[child_index]
				else:
					if node.updated: #an updated node has an assigned attribute
						pass
						#attribute = node.attribute
						#attribute, operator, value, value_index = self.parse_comparison(comparison, attribute=attribute)
						#if self.inverse_operators[operator] == operator:
						#	child = node.add_child(name=comparison)
						#pass
					else:
						attribute, operator, comparable_value, comparable_value_index = self.parse_comparison(comparison)
						node.update(attribute=attribute,
									comparable_value_index=comparable_value_index,
									comparable_value=comparable_value,
									operator=operator)
					child = node.add_child(name=comparison)
				#if n_comparisons == comp_idx:
				node = child
			#print("labels[rule_index]",labels[rule_index])
			#print("rule_index",rule_index)
			node.output_label = labels[rule_index]
		return root_node
	
	def parse_comparison(self,comparison,attribute=None):
		#print("parsing comparison ", comparison)
		
		#Search for the attribute
		if attribute is None:
			for attribute_n,attribute_c in self.attributes.items():
				str_index = comparison.find(attribute_n)
				if str_index != -1:
					attribute = attribute_c
					break
		else:
			str_index = comparison.find(attribute.name)
			if str_index == -1:
				print("Error, wrong attribute")
			
		if attribute is None:
			print("Error, no attribute matched")
			
		#Get variables
		attribute_name = attribute.name
		remainder = comparison[str_index+len(attribute_name):].strip()
		operator = self.operators_db[remainder.split(" ")[0]]
		comparable_value = float(remainder.split(" ")[1]) #all are floats because of this
		
		#Add to the model
		self.add_operator(operator)
		comparable_value_index = add_crucial_value(attribute,comparable_value)
		
		return attribute, operator, comparable_value, comparable_value_index
