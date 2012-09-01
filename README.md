tiny_pq : A priority queue based on gb_trees
--

tiny_pq stores values associated with a priority, and provides functions for
purging low-priority items efficiently. Internally, tiny_pq uses gb_trees,
so you are welcome to use any gb_trees functions as well. tiny_pq is ideal
for services with some notion of expiration, e.g. sessions, caches, message
queues, etc.
