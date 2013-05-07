Alden Quimby
adq2101
CS 4701 Assignment 4 - Decision Tree

---------------------
TO RUN MY CODE
---------------------
	- On a Windows machine, open a command prompt and navigate to this folder
	- To run the learner, type "DecisionTreeLearner.exe restaurant_train.csv"
		- Or specify other training data as the first argument
		- Use a full rooted path, or just the file name if it is in the same
		  directory as the exe
	- The learner will run the classifier for you if you'd like
		- Enter "restaurant_test.csv" when it prompts for arguments
	- Or you can run the classifier separately after running the learner
	  by typing "DecisionTreeClassifier.exe restaurant_test.csv"
		- Again, specify other test data as the first argument
		- Use a full rooted path, or just the file name if it is in the same
		  directory as the exe

---------------------
TO READ MY CODE
---------------------
	- DecisionTree
		- Contains model objects shared by learner/classifier
		- All files are commented with explanations
	- DecisionTreeLearner
		- Learner.cs    --> primary decision tree learning implmentation
		- Compiler.cs   --> helper for creating executable classifier
	- DecisionTreeClassifier
		- Classifier.cs --> primary decision tree classifier implementation

---------------------
TEST CASE OUTPUT
---------------------
	- When running the learner/classifier with restaurant_train.csv and
	  restaurant_test.csv, I get the following output from the classifier:

		Some,Nope,Yeah,Nope,$$,Yeah,French,10-30   ==> Yes
		None,Yeah,Yeah,Nope,$$$,Yeah,Burger,>60    ==> No
		Full,Yeah,Yeah,Yeah,$$$,Yeah,Italian,>60   ==> Maybe

---------------------
CODE GENERATION NODES
---------------------
	- I chose to write the majority of the classifier before hand, and have
	  the learner modify one line of the classifier and recompile it every
	  time it runs
	- The learner builds the decision tree, JSON serializes it (dependecy on
	  Netwonsoft.Json.dll) and sticks it into the main file of the classifier
	- When the classifier runs, it deserializes the tree and classifies
	- This roughly follows one of Adrian's suggestions on Piazza:
		- https://piazza.com/class#spring2013/comsw4701/108
		- "your program could output a program which contains a serialized
		   representation of the tree you just learn."
	- Note that I used the Microsoft.Build namespace to compile the classifier
	  from the learner, and had to tweak the .csproj file to get this to work

---------------------
QUICK EFFICIENCY NOTE
---------------------
	- Because the decision tree algorithm runs very fast, I chose to sort
	  attributes by maximizing information gain for completeness, instead of 
	  minimizing remaining entropy
	- To improve efficiency, it would be a trivial change to sort attributes
	  by minimizing entropy, and it would not effect the results of the learner
	- Specifically, make the following 1 line replacement:

var attr = attributes.OrderByDescending(x => Importance(examples, x)).First();
var attr = attributes.OrderBy(x => Remainder(examples, x)).First();

	- Again, I chose not to do this just to show the full "information gain"
	  aspect, but either one would totally work