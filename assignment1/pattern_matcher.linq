<Query Kind="Program">
  <Namespace>System.Web</Namespace>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Collections.Specialized</Namespace>
</Query>

void Main()
{
	DoTest(new List<string>(), new List<string>(), ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"ai"}, new List<string>{"ai"}, ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"ai","cs"}, new List<string>{"ai","cs"}, ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"cs","ai"}, new List<string>{"ai","cs"}, ResultType.NoMatch);
	
	DoTest(new List<string>{"1","2","3","0"}, new List<string>{"1","2","3","4","0"}, ResultType.NoMatch);
	
	DoTest(new List<string>{"?","mudd"}, new List<string>{"seely","mudd"}, ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"?first","?middle","mudd"}, new List<string>{"seely","w","mudd"}, ResultType.MatchWithVariables, new List<Tuple<string, string>>{Tuple.Create("?middle", "w"),Tuple.Create("?first","seely")});
	
	DoTest(new List<string>{"?","?x","?","?y","?"}, new List<string>{"Warren","bu","is","a","good","ma"}, ResultType.NoMatch);
	
	DoTest(new List<string>{"School","Of","Engineering","and","Applied","Science"}, new List<string>{"School","Of","Engineering"}, ResultType.NoMatch);
	
	DoTest(new List<string>{"*","School","Of","Engineering","and","Applied","Science"}, new List<string>{"The","Foo","Foundation","School","Of","Engineering","and","Applied","Science"}, ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"The","*","School","Of","Engineering","and","Applied","Science"}, new List<string>{"The","School","Of","Engineering","and","Applied","Science"}, ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"A","*","B"}, new List<string>{"A","A","A","A","A","B"}, ResultType.MatchNoVariables);
	
	DoTest(new List<string>{"*","3","?x","4","*"}, new List<string>{"3","5","4"}, ResultType.MatchWithVariables, new List<Tuple<string, string>>{Tuple.Create("?x", "5")});
	
	DoTest(new List<string>{"a","*","?x","*"}, new List<string>{"a","b","c","d"}, ResultType.MatchWithVariables, new List<Tuple<string, string>>{Tuple.Create("?x", "b"),Tuple.Create("?x", "c"),Tuple.Create("?x", "d")});
	
	DoTest(new List<string>{"?x","*","?y"}, new List<string>{"A","A","A","A","A","B"}, ResultType.MatchWithVariables, new List<Tuple<string, string>>{Tuple.Create("?y","b"),Tuple.Create("?x","a")});
}

public enum ResultType
{
	NoMatch,
	MatchNoVariables,
	MatchWithVariables,
}

public class Result
{
	public Result(ResultType type)
	{
		Associations = new List<Tuple<string, string>>();
		Type = type;
	}

	public ResultType Type {get;set;}
	public List<Tuple<string, string>> Associations {get;set;}
}

public Result Match(List<string> pattern, List<string> data)
{
	// base case 1: pattern is empty	
	if (pattern.Count == 0)
	{
		if (data.Count == 0)
		{
			return new Result(ResultType.MatchNoVariables);
		}
		else 
		{
			return new Result(ResultType.NoMatch);
		}
	}
	
	// base case 2: data is empty
	if (data.Count == 0)
	{
		// we have a match if only *s remain in pattern
		if (pattern.All(x => x.Equals("*")))
		{
			return new Result(ResultType.MatchNoVariables);
		}
		else
		{
			return new Result(ResultType.NoMatch);
		}
	}
	
	// at this point, data and pattern are non-empty
	
	var p = pattern[0];
	var d = data[0];
	
	if (p.Equals(d) || p.Equals("?"))
	{
		return Match(pattern.Cdr(), data.Cdr());
	}
	else if (p.StartsWith("?"))
	{
		var subResult = Match(pattern.Cdr(), data.Cdr());
		
		if (subResult.Type == ResultType.NoMatch)
		{
			return subResult;
		}
		else
		{
			subResult.Type = ResultType.MatchWithVariables;
			subResult.Associations.Add(Tuple.Create(p, d));
			return subResult;
		}
	}
	else if (p.Equals("*"))
	{
		var starHasNoData = Match(pattern.Cdr(), data);
		var starHasData = Match(pattern, data.Cdr());
	
		if (starHasNoData.Type == ResultType.NoMatch)
		{
			return starHasData; // one failed, return other
		}
		if (starHasData.Type == ResultType.NoMatch)
		{
			return starHasNoData; // one failed, return other
		}
		
		// both worked!
		
		// no variables only if neither had variables
		var type = starHasData.Type == ResultType.MatchNoVariables && starHasNoData.Type == ResultType.MatchNoVariables
						? ResultType.MatchNoVariables
						: ResultType.MatchWithVariables;
		
		var result = new Result(type);
		result.Associations = starHasData.Associations.Concat(starHasNoData.Associations).ToList();
		return result;
	}
	else
	{
		return new Result(ResultType.NoMatch);
	}
}

private void DoTest(List<string> pattern, List<string> data, ResultType type, List<Tuple<string, string>> assoc = null)
{
	var input = string.Format("\nPattern: ({0})\nData: ({1})", string.Join(" ", pattern), string.Join(" ", data));

	var result = Match(pattern, data);
	if (result.Type != type)
	{
		string.Format("\n********\nTEST FAILED\n********{0}\nExpected: {1}\nActual: {2}", input, type, result.Type).Dump();
		return;
	}
	
	if (result.Type == ResultType.MatchWithVariables)
	{
		var compResult = CompareLists(assoc, result.Associations);
		if (!compResult.Item1)
		{
			compResult.Item2.Dump();
			return;
		}
	}
	
	string.Format("\nTEST PASSED{0}", input).Dump();
}

private Tuple<bool, string> CompareLists(List<Tuple<string, string>> list1, List<Tuple<string, string>> list2)
{
	if (list1.Count != list2.Count)
	{
		return Tuple.Create(false, "wrong length");
	}

	var d1 = list1.ToLookup(x => x.Item1, x => x.Item2).ToDictionary(x => x.Key, x => new HashSet<string>(x));
	var d2 = list2.ToLookup(x => x.Item1, x => x.Item2).ToDictionary(x => x.Key, x => new HashSet<string>(x));

	if (d1.Count != d2.Count)
	{
		return Tuple.Create(false, "wrong num keys");
	}
	
	foreach (var kvp in d1)
	{
		if (!d2.ContainsKey(kvp.Key))
		{
			return Tuple.Create(false, "missing key");
		}
		
		var l1 = kvp.Value;
		var l2 = d2[kvp.Key];
		
		if (l1.Count != l2.Count)
		{
			return Tuple.Create(false, "wrong num values");
		}
		
		if (!l1.All(x => l2.Contains(x)))
		{
			return Tuple.Create(false, "weird");
		}
	}
	
	return Tuple.Create(true, "");
}

public static class Extensions
{
	public static T Pop<T>(this List<T> list)
	{
		var t = list[0];
		list.RemoveAt(0);
		return t;
	}
	
	public static List<T> Cdr<T>(this List<T> list)
	{
		return list.GetRange(1, list.Count - 1);
	}
}
