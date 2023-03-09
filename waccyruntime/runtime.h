struct array
{
  void **data;
  unsigned elemsize;
  unsigned size;
};

unsigned array_size(struct array *in);