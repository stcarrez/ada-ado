-----------------------------------------------------------------------
--  ado-sequences -- Database sequence generator
--  Copyright (C) 2009 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log;
with Util.Log.Loggers;
with Util.Strings;
with ADO.Sessions.Factory;
with Ada.Unchecked_Deallocation;
package body ADO.Sequences is

   use Util.Log;
   use Sequence_Maps;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Sequences");

   procedure Free is new
     Ada.Unchecked_Deallocation (Object => Sequence_Generator,
                                 Name   => Sequence_Generator_Access);

   procedure Free is new
     Ada.Unchecked_Deallocation (Object => Generator'Class,
                                 Name   => Generator_Access);

   --  ------------------------------
   --  Get a session to connect to the database.
   --  ------------------------------
   function Get_Session (Gen : in Generator) return ADO.Sessions.Master_Session'Class is
   begin
      return Gen.Factory.Get_Master_Session;
   end Get_Session;

   protected body Sequence_Generator is

      --  ------------------------------
      --  Allocate a unique identifier for the given sequence.
      --  ------------------------------
      procedure Allocate (Session : in out Sessions.Master_Session'Class;
                          Id      : out Identifier) is
      begin
         Generator.Allocate (Session, Id);
      end Allocate;

      procedure Set_Generator (Gen : in Generator_Access) is
      begin
         Free (Generator);
         Generator := Gen;
      end Set_Generator;

   end Sequence_Generator;

   --  ------------------------------
   --  Allocate a unique identifier for the given table.
   --  ------------------------------
   procedure Allocate (Manager : in out Factory;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Id      : in out ADO.Objects.Object_Record'Class) is
      Gen  : Sequence_Generator_Access;
      Name : constant Util.Strings.Name_Access := Id.Get_Table_Name;
      Key  : Identifier;
   begin
      Manager.Map.Get_Generator (Name.all, True, Gen);
      Gen.Allocate (Session, Key);
      Id.Set_Key_Value (Key);
   end Allocate;

   procedure Allocate (Manager : in out Factory;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Name    : in String;
                       Id      : out Identifier) is
      Gen  : Sequence_Generator_Access;
   begin
      Manager.Map.Get_Generator (Name, True, Gen);
      Gen.Allocate (Session, Id);
   end Allocate;

   --  ------------------------------
   --  Set a generator to be used for the given sequence.
   --  ------------------------------
   procedure Set_Generator (Manager : in out Factory;
                            Gen     : in Generator_Access) is
   begin
      Manager.Map.Set_Generator (Gen);
   end Set_Generator;

   procedure Set_Generator (Manager : in out Factory;
                            Name    : in String;
                            Gen     : in String) is
   begin
      Manager.Map.Set_Generator (Name, Gen);
   end Set_Generator;

   --  ------------------------------
   --  Set the default factory for creating generators.
   --  The default factory is the HiLo generator.
   --  ------------------------------
   procedure Set_Default_Generator
     (Manager      : in out Factory;
      Factory      : in Generator_Factory;
      Sess_Factory : in Session_Factory_Access;
      Use_New_Session : in Boolean) is
   begin
      Manager.Map.Set_Default_Generator (Factory, Sess_Factory, Use_New_Session);
   end Set_Default_Generator;

   --  ------------------------------
   --  Set the name of a generator to be used by default when a generator is
   --  not found or was not configured by using `Set_Generator`.
   --  ------------------------------
   procedure Set_Default_Generator (Manager : in out Factory;
                                    Name    : in String) is
   begin
      Manager.Map.Set_Default_Generator (Name);
   end Set_Default_Generator;

   --  The sequence factory map is also accessed through a protected type.
   protected body Factory_Map is

      --  ------------------------------
      --  Get the sequence generator associated with the name.
      --  If there is no such generator, an entry is created by using
      --  the default generator.
      --  ------------------------------
      procedure Get_Generator (Name : in String;
                               Check_Alias : in Boolean;
                               Seq  : out Sequence_Generator_Access) is
         Pos : constant Cursor := Find (Map, Name);
      begin
         if Has_Element (Pos) then
            Seq := Element (Pos);
            return;
         end if;
         if Check_Alias then
            declare
               Alias_Pos : constant Util.Strings.Maps.Cursor
                 := Util.Strings.Maps.Find (Alias, Name);
            begin
               if Util.Strings.Maps.Has_Element (Alias_Pos) then
                  Get_Generator (Util.Strings.Maps.Element (Alias_Pos), False, Seq);
                  return;
               end if;
            end;
         end if;

         Log.Info ("Creating sequence generator for {0}", Name);
         declare
            Generator : constant Generator_Access
              := Create_Generator.all (Sess_Factory, Name);
         begin
            Generator.Use_New_Session := Use_New_Session;
            Seq := new Sequence_Generator;
            Seq.Set_Generator (Generator);
         end;
         Insert (Map, Name, Seq);
      end Get_Generator;

      --  ------------------------------
      --  Set the sequence generator associated with the name.
      --  ------------------------------
      procedure Set_Generator (Gen : in Generator_Access) is
         Pos : constant Cursor := Find (Map, Gen.Name);
      begin
         Log.Info ("Setting sequence generator for {0}", Gen.Name);

         if not Has_Element (Pos) then
            declare
               Seq : constant Sequence_Generator_Access := new Sequence_Generator;
            begin
               Seq.Set_Generator (Gen);
               Insert (Map, Gen.Name, Seq);
            end;
         else
            declare
               Seq : constant Sequence_Generator_Access := Element (Pos);
            begin
               Seq.Set_Generator (Gen);
            end;
         end if;
      end Set_Generator;

      procedure Set_Generator (Name : in String;
                               Gen  : in String) is
      begin
         Log.Info ("Use sequence generator {0} for {1}", Gen, Name);

         Alias.Include (Name, Gen);
      end Set_Generator;

      --  ------------------------------
      --  Set the default sequence generator.
      --  ------------------------------
      procedure Set_Default_Generator
        (Gen     : in Generator_Factory;
         Factory : in Session_Factory_Access;
         Gen_Use_New_Session : in Boolean) is
      begin
         Create_Generator := Gen;
         Sess_Factory := Factory;
         Use_New_Session := Gen_Use_New_Session;
      end Set_Default_Generator;

      procedure Set_Default_Generator (Name : in String) is
      begin
         Default_Generator := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      end Set_Default_Generator;

      --  ------------------------------
      --  Clear the factory map.
      --  ------------------------------
      procedure Clear is
      begin
         Log.Info ("Clearing the sequence factory");

         loop
            declare
               Pos : Cursor := Map.First;
               Seq : Sequence_Generator_Access;
            begin
               exit when not Has_Element (Pos);
               Seq := Element (Pos);
               Map.Delete (Pos);
               Seq.all.Set_Generator (null);
               Free (Seq);
            end;
         end loop;
      end Clear;

   end Factory_Map;

   overriding
   procedure Finalize (Manager : in out Factory) is
   begin
      Manager.Map.Clear;
   end Finalize;

end ADO.Sequences;
