with Sax.Readers;

package Kit.Sax_Reader is

   type Reader is
     new Sax.Readers.Reader
   with private;

private

   type Reader is
     new Sax.Readers.Reader with null record;

end Kit.Sax_Reader;
