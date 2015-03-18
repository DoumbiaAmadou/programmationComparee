#include <iostream>
#include <math.h>
#include <assert.h>
using namespace std;

class PrimeChecker {
  virtual bool is_prime (int x) = 0;
};

class Eratosthene : public PrimeChecker {

private:
      int num;
      bool* prime;

public:
      Eratosthene (int num) {
            this->num = num;
            prime = new bool[num];
            for(int i = 0; i < num; i++){ //sets all array to true for the marking
                  prime[i] = true;
            }
      }

      void printPrime() {
            for(int i = 2; i < num; i++){
                  if(prime[i] == true){
                        cout << i << endl;
                  }
            }
      }

      int eratosthene(){

            for(int i = 2; i < num; i++){
                  if(i <= sqrt(num) && prime[i] == true){
                        for(int j = i*2; j < num; j+=i){
                              prime[j] = false;
                        }
                  }
            }
            printPrime();

            delete [] prime;

            return 0;
      }
};

int main() {
      int number;
      int stop = 5;
      int i = 0;
      while (i < stop) {
            cout << "Enter a number: ";
            cin >> number;

            Eratosthene *eratos = new Eratosthene(number);
            eratos->eratosthene();
            delete [] eratos;
      }
      return 0;
}
