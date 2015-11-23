(* Strings:*)

(* KMP *)
(* Rabin-Karp 2D *)
(* AC Automaton *)
(* Z-algorithm *)

(* Given a string S of length n, the Z Algorithm produces an array Z where 
Z[i] is the length of the longest substring starting from S[i] which is 
also a prefix of S, i.e. the maximum k such that S[j] = S[i + j] for all 0 
≤ j < k.

string -> int -> int array

Input: A string s and its size n
Output: Z array

Complexity: O(n) *)

let z_algorithm s n =
    let l = ref 0 and r = ref 0 and z = Array.make n 0 in
    let update i =
        l := i;
        while !r < n && s.[!r - !l] = s.[!r] do
            incr r;
        done;
        z.(i) <- !r - !l;
        decr r 
    in
        for i=1 to n-1 do
            if i > !r
                then (r := i; update i)
                 else if z.(i - !l) < !r - i + 1
                          then z.(i) <- z.(i - !l)
                          else update i;
        done;
    z

(* Hash function for string *)

(* Optimization: *)

(* Sparse Max-flow *)
(* Min-cost max-flow *)
(* Push-relabel max-flow *)
(* Min-cost matching *)
(* Max bipartite matching *)
(* Stable matchings *)
(* Global Min-cut *)

(* Geometry: *)

(* Convex Hull *)
(* Miscellaneous *)
(* Half-plane intersection *)
(* Closest Pair *)
(* Delaunay Triangulation *)

(* Numerical Algorithms: *)

(* Number theoretic algorithm *)
(* Discrete Logarithm *)
(* Euler's function *)
(* Systems of linear equations, matrix inverse, determinant *)
(* Reduced row echelon form, matrix rank *)
(* Fast Fourier Transform *)
(* Linear Programming *)
(* Binomal coefficient *)
(* Sieve of Erathostenes *)
(* Prime number *)

(* Graphs: *)

(* Dijkstra's algorithm *)
(* Bellman-Ford *)
(* MST *)
(* Strongly connected components *)
(* DFS *)
(* BFS *)
(* Topological Sort *)
(* Eulerian Circuit *)
(* Test Bipartite Graph *)
(* Cut vertex/Bridge *)
(* Biconnected Component *)
(* Exact Cover *)
(* Lowest Common Ancestor *)

(* Data Structures: *)

(* Suffix Arrays *)
(* Binary Indexed Tree *)
(* Union-Find *)
(* KD-tree *)
(* Segment Tree *)
(* Treap *)
(* Bits Sets *)
(* Trie *)
(* Rank Tree *)

(* Miscellaneous: *)

(* Longest Increasing Subsequence *)
(* Maximum Rectangle in a Matrix *)
(* Dichotomy *)
(* 2-SAT *)
