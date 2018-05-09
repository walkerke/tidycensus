LeafletWidget.methods.createMapPane = function (name, zIndex) {

  map = this;
  map.createPane(name);
  map.getPane(name).style.zIndex = zIndex;

};
