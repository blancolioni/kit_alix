with Ada.Strings.Fixed;

with AWS.Response;
with AWS.Server;
with AWS.Status;

with Kit.Server.Http.Root;
with Kit.Server.Http.Table_Page;

package body Kit.Server.Web_Server is

   function Service
     (Request : AWS.Status.Data)
     return AWS.Response.Data;

   function Starts_With (S : String; Head : String) return Boolean
   is (Ada.Strings.Fixed.Index (S, Head) = S'First);

   -------------
   -- Service --
   -------------

   function Service
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      URI : constant String :=
              AWS.Status.URI (Request);
   begin
      if URI = "/" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => Kit.Server.Http.Root.Root_Page);
      elsif Starts_With (URI, "/table/") then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              Kit.Server.Http.Table_Page.Table_Page
                (Name => URI (URI'First + 7 .. URI'Last)));
      else
         return AWS.Response.Build (Content_Type => "text/html",
                                    Message_Body =>
                                      "<p>Requested URI: <em>"
                                    & AWS.Status.URI (Request)
                                    & "</em>");
      end if;
   end Service;

   ----------------------
   -- Start_Web_Server --
   ----------------------

   procedure Start_Web_Server is
      WS : AWS.Server.HTTP;
   begin
      AWS.Server.Start
        (Web_Server => WS,
         Name       => "Kit",
         Callback   => Service'Access,
         Max_Connection => 10,
         Port => 11453);
      AWS.Server.Wait;
   end Start_Web_Server;

end Kit.Server.Web_Server;
