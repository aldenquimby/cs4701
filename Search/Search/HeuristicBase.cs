namespace Search
{
    public interface IHeuristic
    {
        int Evaluate(StateBase state, StateBase goal);
        string Name { get; }
    }

    public abstract class HeurisitcBase<T> : IHeuristic where T : StateBase
    {
        public int Evaluate(StateBase state, StateBase goal)
        {
            return Evaluate(state as T, goal as T);
        }

        public abstract int Evaluate(T state, T goal);

        public abstract string Name { get; }
    }
}