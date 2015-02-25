#include <iostream>
#include <math.h>
#include <assert.h>
using namespace std;

class Eratosthene {

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

      void eratosthene(){
            for(int i = 2; i < num; i++){
                  if(i <= sqrt(num) && prime[i] == true){
                        for(int j = i*i; j < num; j++){ 
                              
                              if(j % i == 0){  
                                          
                                    prime[j] = false;
                              } 
                        }
                  }
            }

            printPrime();

            delete [] prime;
      }
};

// don't use genrandom
int main() {
      int number;
      cout << "Enter a number: ";
      cin >> number;

      Eratosthene *eratos = new Eratosthene(number);
      eratos->eratosthene();
      delete [] eratos;
      return 0;
}