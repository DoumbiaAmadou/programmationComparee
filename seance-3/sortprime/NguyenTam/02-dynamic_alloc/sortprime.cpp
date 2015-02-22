#include <iostream>
#include <math.h>
#include <assert.h>
using namespace std;

void setDefault(bool* prime, int num){
      for(int i = 0; i < num; i++){ //sets all array to true for the marking
             prime[i] = true;
      }
}

void printPrime(bool* prime, int num) {
     for(int i = 2; i < num; i++){
            if(prime[i] == true){
                  cout << i << endl;
            }
      } 
}

int eratosthene(int num){
      bool* prime = new bool[num];

      setDefault(prime, num);

      for(int i = 2; i < num; i++){
            if(i <= sqrt(num) && prime[i] == true){
                  for(int j = i*i; j < num; j++){ 
                        if(j % i == 0){               
                              prime[j] = false;
                        } 
                  }
            }
      }

      printPrime(prime, num);

      delete [] prime;

      return 0;
}
// dont yet use genrandom
int main() {
      int number;
      cout << "Enter a number: ";
      cin >> number;
      eratosthene(number);
      return 0;
}