#include "tools.h"

//
//  TSP - BRUTE-FORCE
//
// -> la structure "point" est définie dans "tools.h"
// -> tsp_main peut être testé dès les 3 premières fonctions codées
//

double dist(point A, point B) {
	int dx = A.x - B.x;
	int dy = A.y - B.y;
	return sqrt(dx * dx + dy * dy);
}

double value(point *V, int n, int *P) {
	int res = 0;
	for (int i = 0; i < n-1; i++) res += dist(V[P[i]], V[P[i+1]]);
	res += dist(V[P[0]], V[P[n-1]]);
	return res;
}

double tsp_brute_force(point *V, int n, int *Q) {
	int P[n];
	for (int i = 0; i < n; i ++) P[i] = i;
	
	int res = -1;
	do {
		int v = value(V, n, P);
		if (v < res || res == -1) {
			res = v;
			memcpy(Q, P, n * sizeof(int));
			drawTour(V,n,P);
			/* SDL_Delay(500); */
		}
		
	} while(NextPermutation(P, n) && running);

	return res;
}

void MaxPermutation(int *P, int n, int k) {
  ;
  ;
  ;
  return;
}

#define CLEAN_D value_opt(NULL, 0, NULL, 0.0)
double value_opt(point *V, int n, int *P, double w) {
	static double** D = NULL;
	if (!D) {
		D = malloc(sizeof(double*)*n);
		for (int i = 0; i < n ; i++) {
			D[i] = malloc(sizeof(double)*n);
			for (int j = 0; j < n ; j++) D[i][j] = dist(V[i], V[j]);
		}
	} else if (!V && D) {
		for (int i = 0; i < n ; i++) free(D[i]);
		free(D);
		D = NULL;
		return 0;
	}
   	int res = 0;
	for (int i = 0; i < n-1; i++) {
		res += D[P[i]][P[i+1]];
		if (w > 0 && res >= w) return -i;
	}
	res += D[P[0]][P[n-1]];
	return res;
}

double tsp_brute_force_opt(point *V, int n, int *Q) {
	int P[n];
	for (int i = 0; i < n; i ++) P[i] = i;
	
	int res = -1;
	do {
		int v = value_opt(V, n, P, res);
		if (v < 0) continue;
		if (v < res || res == -1) {
			res = v;
			memcpy(Q, P, n * sizeof(int));
			/* drawTour(V,n,P); */
			/* SDL_Delay(500); */
		}
		
	} while(NextPermutation(P + 1, n - 1) && running);
	CLEAN_D;
	return res;
}
