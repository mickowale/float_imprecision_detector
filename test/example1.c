
int main() {
  float a_f = 2244.345;
  double a_d = 24452.345;
  float r_f = a_f * a_f;
  double r_d = a_d * a_d;
  double c = r_d * 1.0;
  r_f = (float) c * (float) 3242;


  return (int) r_f;
}
