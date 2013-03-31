namespace Isolation
{
    public class ClientConfig
    {
        public ClientConfig(string input)
        {
            UseDatabaseHeuristicCache = true;
        }

        public bool UseDatabaseHeuristicCache { get; set; }
    }
}