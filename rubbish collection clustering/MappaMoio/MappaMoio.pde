PrintWriter out;
void setup(){
  out = createWriter("coordinate.txt");
  size(1605,949);
  PImage mappa;
  mappa = loadImage("MoioMap.png");
  image(mappa,0,0);
  
}

void draw(){

}

void mouseClicked(){
  fill(255,0,0);
  ellipse(mouseX, mouseY,2,2);
  out.println(mouseX + "\t" + (mouseY - height)*(-1));
}
 
void keyPressed() {
  out.flush(); // Writes the remaining data to the file
  out.close(); // Finishes the file
  save("pointedMap.png");
  exit(); // Stops the program
}
