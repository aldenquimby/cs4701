using System;
using System.Collections.Generic;

namespace Search
{
    public abstract class StateBase : IEquatable<StateBase>
    {
        public abstract bool Equals(StateBase other);
        public abstract override bool Equals(object obj);
        public abstract override int GetHashCode();
        public abstract IDictionary<string, StateBase> Successors();
    }
}