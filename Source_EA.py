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

def get_arity(operator):
	"""
	Returns the arity of the method, operator or funtion as an int
	:param operator: is a method, operator or funtion
	"""
	sig = signature(operator)
	arity = len(sig.parameters)
	return arity
		
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
	
		self.index = index
		self.name = name
		self.type = type
		self.all_values = all_values
		self.crucial_values = crucial_values
		self.available_values = available_values
	
	def add_crucial_value(self,value):
		if value in self.crucial_values:
			index = self.crucial_values.index(value)
			return index
		else:
			index = len(self.crucial_values)
			self.crucial_values.append(value)
			print("Added crucial value ", str(value), " to attribute ", self.name)
			return index
	
	def __str__(self):
		return "Attribute " + self.name + " idx " + self.index + " n_crucial_values" + str(len(self.crucial_values))  
	
class DT_Node:
	def __init__(self, parent = None):
		self.updated=False
		self.parent=parent
		self.children = []
		self.children_names = []
		self.output_label = None
	
	def update(self,attribute,comparable_value_index,value,operator):
		self.updated = True
		self.attribute = attribute
		self.comparable_value = value #float(self.attribute.crucial_values[comparable_value_index]) #all are floats rn
		self.comparable_value_index = comparable_value_index
		self.operator = operator
	
	def add_child(self,name):
		child = DT_Node(parent=self)
		self.children.append(child)
		self.children_names.append(name)
		print("Added child named ", name)
		return child
	
	def set_output_label(self, output_label):
		self.output_label = output_label
	
	def evaluate(self,data_row):
		#data_row = pd.DataFrame(data_row)
		#"""
		print("data_row",data_row)
		print("node",str(self))
		print("value", str(data_row.iloc[self.comparable_value_index]))
		print("value type", type(data_row.iloc[self.comparable_value_index]))
		print("operator",str(self.operator))
		print("comparable_value",str(self.comparable_value))
		print("comparable_value type",type(self.comparable_value))
		#assert self.operator(self.comparable_value, data_row.iloc[self.comparable_value_index])
		#print("result", str(self.operator(self.comparable_value, data_row.iloc[self.comparable_value_index])))
		#"""
		if self.output_label is None:
			if type(self.comparable_value) == "str" or type(data_row.iloc[self.comparable_value_index]) == "str":
				return "wrong"
			else:
				if self.operator(self.comparable_value, data_row.iloc[self.comparable_value_index]):
					return self.children[0].evaluate(data_row)
				else:
					return self.children[1].evaluate(data_row)
		else:
			return self.output_label
		#"""
	def is_terminal(self):
		return self.output_label is None

	def is_root(self):
		return self.parent is None

	def get_subtree_nodes(self):
		"""
		Returns a list with all the nodes of the subtree with this node as the root node, including himself
		"""
		nodes = [self]
		i = 0
		while i < len(nodes):
			if not nodes[i].is_terminal:
				nodes.extend(nodes[i].children)
			i += 1
		return nodes

	def get_max_depth(self, depth = 0):
		"""
		Returns the max depth of this tree as an int
		"""
		new_depth = depth + 1
		if self.is_terminal:
			return new_depth
		else:
			return max([child.my_depth(new_depth) for child in self.children])

	def copy(self, parent=None):
		"""
		Don't give arguments. Returns an unrelated new item with the same characteristics
		"""
		the_copy = GP_Node(self.content, parent = parent)
		if not self.is_terminal:
			for child in self.children:
				the_copy.children.append(child.copy(parent = the_copy))
		return the_copy

	def __str__(self):
			return "Node " + self.attribute.name + str(self.operator) + str(self.comparable_value)

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
	
	def create_individual_from_R(R_tree):
		
		pass

	def attribute_mutation(self,
			individual):
		pass

	def attribute_value_mutation(self,
			individual):
		pass
			
	def one_point_crossover(self,
			individual1,
			individual2):
		pass
		
	def evaluate_tree(self,root_node,provisional_data=None):
		if provisional_data is None:
			data = self.data
		else:
			data = provisional_data
		output_array = []
		for index, row in data.iterrows():
			if index == 1:
				root_node.evaluate(row)
			
			
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
					if node.updated:
						pass
						#attribute = node.attribute
						#attribute, operator, value, value_index = self.parse_comparison(comparison, attribute=attribute)
						#if self.inverse_operators[operator] == operator:
						#	child = node.add_child(name=comparison)
						#pass
					else:
						attribute, operator, value, value_index = self.parse_comparison(comparison)
						node.update(attribute=attribute,
									comparable_value_index=value_index,
									value=value,
									operator=operator)
					child = node.add_child(name=comparison)
				#if n_comparisons == comp_idx:
				node = child
			node.set_output_label(labels[rule_index])
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
			print("Error, no attribute was matched")
			
		#Get variables
		attribute_name = attribute.name
		remainder = comparison[str_index+len(attribute_name):].strip()
		operator = self.operators_db[remainder.split(" ")[0]]
		value = float(remainder.split(" ")[1]) #all are floats because of this
		
		#Add to the model
		self.add_operator(operator)
		value_index = attribute.add_crucial_value(value)
		
		return attribute, operator, value, value_index

#
