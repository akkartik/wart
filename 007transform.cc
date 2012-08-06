//// transform Cell at read time

typedef Cell* (*transformer)(Cell*);
const transformer transforms[] = {
  #include "transform_list"
};

Cell* transform(Cell* cell) {
  for (unsigned long i=0; i < sizeof(transforms)/sizeof(transforms[0]); ++i)
    cell = (*transforms[i])(cell);
  return cell;
}
