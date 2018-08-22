#include <caml/mlvalues.h>
#include <caml/address_class.h>

CAMLprim value Base_heap_block_is_heap_block(value v)
{
  return (Is_block(v)
          && Is_in_value_area(v)
          && Tag_val (v) != Lazy_tag
          && Tag_val (v) != Double_tag
          && Tag_val (v) != Forward_tag) ? Val_true : Val_false;
}
