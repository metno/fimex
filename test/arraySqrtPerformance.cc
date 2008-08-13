/*
 * arraySqrtPerformance.cc
 *
 *  Created on: Aug 8, 2008
 *      Author: heikok
 */

/**
 * results: (gcc 4.1.1 FC5 -O2 (same for -O3))
 * transform slightly version faster than while version: 283MFLOPS vs 279 MFLOPS
 * openmp (-fopenmp) about 80% faster on Core2 Duo T7300 520MFLOPS
 * -mfpmath=sse -msse2 about 18% faster on Core2 Duo T7300 (238MFLOPS for transform, 239 for while)
 * -mfpmath=sse -msse2 about 14% faster on Xeon (cpu-family 15, model 2) (dual core, 2.35MHz) (360MFLOPS vs 317MFLOPS)
 *
 *
 * translated to java/transform version:
 * Core2 Duo: java 1.6.0_07 server: 282MFLOPS client: 275MFLOPS, same results for 1.5.0_06
 * Xeon: java 1.6.0_06 server: 363 MFLOPS client: 225MFLOPS
 */
#include <vector>
#include <sys/time.h>
#include <cmath>
#include <iostream>
#include <sstream>

using namespace std;

class Calc : public std::binary_function<double, double, double>
{
	double offset;
public:
	Calc(double offset) : offset(offset) {}
	double operator() (double old1, double old2) {
		double new1 = old1 + offset;
		double new2 = old2 - offset;
		if (offset < 1) {
			// normalize
			double norm = sqrt((old1*old1 + old2*old2)/(new1*new1 + new2*new2));
			new1 *= norm;
			new2 *= norm;
		}
		return new1;
	}
};
void calcByTransform(vector<double>& values1, vector<double>& values2) {
	// this doesn't change the values of values2, but good enough for testing
#pragma omp parallel for
	for (int i = 0; i < 10; i++) {
		transform(values1.begin(), values1.end(), values2.begin(), values1.begin(), Calc(i*.02));
	}
}
void calcByWhile(vector<double>& values1, vector<double>& values2) {
	int size = values1.size();
#pragma omp parallel for
	for (int i = 0; i < 10; i++) {
		double offset = i * 0.02;
		double* curVal1 = &values1[0];
		double* curVal2 = &values2[0];
		double* endVal1 = &values1[values1.size()];
		while (curVal1 != endVal1) {
			double new1 = *curVal1 + offset;
			double new2 = *curVal2 - offset;
			if (i < 20) {
				// normalize
				double norm = sqrt((*curVal1 * *curVal1 + *curVal2 * *curVal2)/(new1*new1 + new2*new2));
				new1 *= norm;
				new2 *= norm;
			}
			*curVal1++ = new1;
			*curVal2++;
			//*curVal2++ = new2;
		}
	}
}

void printStat(const vector<double>& stats, int flop) {
	double min = stats[0];
	double max = stats[0];
	for (int i = 0; i < stats.size(); i++) {
		min = (stats[i] < min) ? stats[i] : min;
		max = (stats[i] > max) ? stats[i] : max;
	}
	double sum = 0;
	int num = 0;
	for (int i = 0; i < stats.size(); i++) {
		if (!((stats[i] == min) || (stats[i] == max))) {
			sum += stats[i];
			num++;
		}
	}
	double avg = sum / num;
	double var = 0;
	if (num > 1) {
		for (int i = 0; i < stats.size(); i++) {
			if (!((stats[i] == min) || (stats[i] == max))) {
				double diff = avg - stats[i];
				var += (diff * diff);
			}
		}
		//cerr << sqrt(var) << " " << num << " " << min << " " << max<< endl;
		var = sqrt(var)/(num-1);
	}
	int flops = static_cast<int>(flop / avg / 1000000);
	cout << avg << "+-" << var << " " << flops << "MFLOPS" << endl;
}

int main(int argc, char **argv) {
	void (*func)(vector<double>&, vector<double>&) = calcByWhile;
	if (argc > 1) {
		std::string transform("transform");
		if (transform == argv[1]) {
			func = calcByTransform;
			cout << "transform ";
		} else {
			cout << "while " ;
		}
	}
	long size = 1000*1000;
	if (argc > 2) {
		stringstream ss;
		ss << argv[2];
		ss >> size;
	}
	cout << size << endl;
	vector<double> values1(size, 1.);
	vector<double> values2(size, 1.);
	vector<double> stats;
	for (int i = 0; i < 10; i++) {
		timeval start, end;
		gettimeofday(&start, 0);
		func(values1, values2);
		gettimeofday(&end, 0);
		stats.push_back((end.tv_sec + (end.tv_usec/1000000.)) - (start.tv_sec + (start.tv_usec/1000000.)));
		cerr << "done: " << i << endl;
	}
	int flop = size * 12 * 10; // 12 * 10 floating point operations in loop
	printStat(stats, flop);
}

