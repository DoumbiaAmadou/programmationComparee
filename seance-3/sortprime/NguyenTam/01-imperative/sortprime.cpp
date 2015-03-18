#include <iostream>
#include <math.h>
#include <assert.h>
using namespace std;

const int MEM_SIZE = 640*1024;    
const int MAX_NUM = (MEM_SIZE)/(sizeof (bool));
bool prime[MAX_NUM];
int nbPrimes[MAX_NUM];

void setDefault(){
      for(int i = 2; i < MAX_NUM; i++){    //sets all array to true for the marking
            prime[i] = true;
            nbPrimes[i] = 0;
      }
}

void printPrime (int n) {
      if (prime[n]) {
          cout << n << " is a prime" << endl; 
          nbPrimes[n] = 1;
          cout << nbPrimes[n] << endl;
      }
}

void printListPrime() {
      for (int i = 2; i < MAX_NUM; i++)
            if (nbPrimes[i] == 1)
                  cout << i << " " << endl;
}

int eratosthene(int num){

      setDefault();

      for(int i = 2; i < MAX_NUM; i++){
            if(i <= sqrt(MAX_NUM) && prime[i] == true){
                  for(int j = i*2; j < MAX_NUM; j+=i){            
                        prime[j] = false;
                  }
            }
      }
      printPrime(num);

      return 0;
}

int main() {
      int number;
      int stop = 5;
      int i = 0;
      while (i < stop) {
            cout << "Enter a number: ";
            cin >> number;
            assert (number < MAX_NUM);
            eratosthene(number);
            i++;
      }
      printListPrime();

      return 0;
}