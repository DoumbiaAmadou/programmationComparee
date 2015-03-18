#include <iostream>
#include <math.h>
#include <assert.h>
using namespace std;

void setDefault(bool* prime, int num){
      for(int i = 0; i < num; i++){ //sets all array to true for the marking
             prime[i] = true;
      }
}

void printPrime (bool* prime, int n) {
      if (prime[n]) {
          cout << n << " is a prime" << endl;
      }
}

int eratosthene(int num){
      bool* prime = new bool[num];
      setDefault(prime, num);

      for(int i = 2; i < num; i++){
            if(i <= sqrt(num) && prime[i] == true){
                  for(int j = i*2; j < num; j+=i){
                        prime[j] = false;
                  }
            }
      }
      printPrime(prime, num);

      delete [] prime;

      return 0;
}

int main() {
      int number;
      int stop = 5;
      int i = 0;
      while (i < stop) {
            cout << "Enter a number: ";
            cin >> number;
            eratosthene(number);
            i++;
      }

      return 0;
}
