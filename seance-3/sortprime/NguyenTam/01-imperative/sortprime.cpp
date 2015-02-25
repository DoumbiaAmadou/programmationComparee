#include <iostream>
#include <math.h>
#include <assert.h>
using namespace std;

const int MEM_SIZE = 640*1024;    //copied from Fontaine
const int MAX_NUM = (MEM_SIZE)/(sizeof (bool));

void setDefault(bool prime[]){
      for(int i = 0; i < MAX_NUM; i++){    //sets all array to true for the marking
             prime[i] = true;
      }
}

void printPrime(bool prime[], int num) {
     for(int i = 2; i < num; i++){
            if(prime[i] == true){
                  cout << i << endl;
            }
      } 
}

int eratosthene(int num){
      bool prime[MAX_NUM];

      setDefault(prime);

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

      return 0;
}

// dont yet use genrandom
int main() {
      int number;
      cout << "Enter a number: ";
      cin >> number;
      assert (number < MAX_NUM);
      eratosthene(number);
      return 0;
}