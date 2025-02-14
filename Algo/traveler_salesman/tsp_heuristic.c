#include "tools.h"
#include "tsp_brute_force.h"

//
//  TSP - HEURISTIQUES
//

void reverse(int *T, int p, int q) {
  // Renverse la partie T[p]...T[q] du tableau T avec p<q si
  // T={0,1,2,3,4,5,6} et p=2 et q=5, alors le nouveau tableau T sera
  // {0,1, 5,4,3,2, 6}.
	int m = (q + p) / 2;
	for (int i = 0; i < m; i++) {
		int tmp = T[i];
		T[i] = T[q-i];
		T[q-i] = tmp;
	}
	return;
}

double first_flip(point *V, int n, int *P) {
	// Renvoie le gain>0 du premier flip réalisable, tout en réalisant
	// le flip, et 0 s'il n'y en a pas.
	for (int i = 0; i < n-1; i++) {
		for (int j = i+2; j < n-1; j++) {
			int d1 = dist(V[P[i]], V[P[i+1]]) + dist(V[P[j]], V[P[j+1]]);
			int d2 = dist(V[P[i]], V[P[j]]) + dist(V[P[i+1]], V[P[j+1]]);
			if (d2 < d1) {
				reverse(P, i, j);
				return d1 - d2;
			}
		}
	}
	return 0.0;
}

#define INIT for (int i = 0; i < n; i++) P[i] = i;

double tsp_flip(point *V, int n, int *P) {
	// La fonction doit renvoyer la valeur de la tournée obtenue. Pensez
	// à initialiser P, par exemple à P[i]=i. Pensez aussi faire
	// drawTour() pour visualiser chaque flip.
	INIT
	while (first_flip(V, n, P)) drawTour(V, n, P);
	return 0.0;
}

double tsp_greedy(point *V, int n, int *P) {
	INIT
	double res = 0;	
	for (int i = 0; i < n-1; i ++) {
		int min = dist(V[P[i]], V[P[i+1]]);
		int index = i+1;
		for (int j = i+1; j < n; j++) {
			int d = dist(V[P[i]], V[P[j]]);
			if (d < min) {
				min = d;
				index = j;
			}
		}
		res += min;
		int tmp = P[i+1];
		P[i+1] = index;
		P[index] = tmp;
	}
		
	return res;
}
