// Test program for sgt code

// Reads a file in which each line contains an observed value and the
// frequency of the value. Prints out a table of the estimates, and also the
// estimate for value 1 (to test the estimate() function).

#include "sgt.h"
#include <iostream>

// Set this to the type for the observation
//typedef double Obs;
typedef unsigned int Obs;

int main()
{
    SGT<Obs> sgt;
    Obs observation;
    unsigned int frequency;
    while (cin >> observation)
    {
        if (!(cin >> frequency))
        {
            cerr << "Incomplete input" << endl;
            return -1;
        }

        sgt.add(observation, frequency);
    }

    sgt.analyse();
    cout << "Results:" << endl;

    // Use iterators to access the results
    pair<SGT<Obs>::iterator, SGT<Obs>::iterator> i = sgt.iterate();
    for (; i.first != i.second; ++i.first)
    {
        cout << sgt.obs(i.first)
            << "\t" << sgt.freq(i.first)
            << "\t" << sgt.estimate(i.first)
            << "\t" << sgt.estimate(i.first) * sgt.total()
            << endl;
    }

    double estimate;
    sgt.estimate(0, estimate);
    cout << "0\t" << estimate << endl;

    if (sgt.estimate(1, estimate))
        cout << "Estimate on obs=1: " << estimate << endl;
    else
        cout << "No estimate for obs=1" << endl;
    return 0;
}

