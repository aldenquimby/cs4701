<Query Kind="Program">
  <Namespace>System.Web</Namespace>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Collections.Specialized</Namespace>
</Query>

void Main()
{
	
}

public class ProblemSolvingAgent
{
	public static Action Perceive(Percept percept)
	{
		State = UpdateState(State, percept);
		
		if (Seq.Count == 0)
		{
			Goal = FormulateGoal(State);
			Problem = FormulateProblem(State, Goal);
			Seq = Search(Problem);
		}
		
		var action = Seq.First();
		Seq = Seq.Rest();
		return action;
	}

	private static List<Action> Seq {get;set;}
	private static State State {get;set;}
	private static Goal Goal {get;set;}
	private static Problem Problem {get;set;}
	
	private static State UpdateState(State current, Percept percept)
	{
		return new State();
	}
	
	private static Goal FormulateGoal(State state)
	{
		return new Goal();
	}
	
	private static Problem FormulateProblem(State state, Goal goal)
	{
		return new Problem();
	}
	
	private static List<Action> Search(Problem problem)
	{
		return new List<Action>();
	}
}

public class Percept
{

}

public class Problem
{

}

public class State
{

}

public class Goal
{

}

public static class Extensions
{
	public static List<T> Rest<T>(this List<T> list)
	{
		if (list == null || list.Count == 0)
		{
			return null;
		}
		
		return list.GetRange(1, list.Count - 1);
	}
}

