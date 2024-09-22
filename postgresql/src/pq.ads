-----------------------------------------------------------------------
--  pq -- Postgresql libpq thin binding
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Interfaces.C.Strings;
with System;
package PQ is

   pragma Linker_Options ("-lpq");

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   type PGconn_Access is new System.Address;

   Null_PGconn : constant PGconn_Access := PGconn_Access (System.Null_Address);

   type PGresult_Access is new System.Address;

   Null_PGresult : constant PGresult_Access := PGresult_Access (System.Null_Address);

   type ConnStatusType is
     (
      CONNECTION_OK,
      CONNECTION_BAD,
      --  Non-blocking mode only below here

      --  The existence of these should never be relied upon - they should only
      --  be used for user feedback or similar purposes.

      --  Waiting for connection to be made.
      CONNECTION_STARTED,

      --  Connection OK; waiting to send.
      CONNECTION_MADE,

      --  Waiting for a response from the postmaster.
      CONNECTION_AWAITING_RESPONSE,

      --  Received authentication; waiting for backend startup.
      CONNECTION_AUTH_OK,

      --  Negotiating environment.
      CONNECTION_SETENV,

      --  Negotiating SSL.
      CONNECTION_SSL_STARTUP,

      --  Internal state: connect() needed
      CONNECTION_NEEDED
     );

   type ExecStatusType is
     (
      --  empty query string was executed
      PGRES_EMPTY_QUERY,

      --  a query command that doesn't return anything was executed properly by the backend
      PGRES_COMMAND_OK,

      --  a query command that returns tuples was executed properly by the backend, PGresult
      --  contains the result tuples
      PGRES_TUPLES_OK,

      --  Copy Out data transfer in progress
      PGRES_COPY_OUT,

      --  Copy In data transfer in progress
      PGRES_COPY_IN,

      --  an unexpected response was recv'd from the backend
      PGRES_BAD_RESPONSE,

      --  notice or warning message
      PGRES_NONFATAL_ERROR,

      --  query failed
      PGRES_FATAL_ERROR,

      --  Copy In/Out data transfer in progress
      PGRES_COPY_BOTH,

      --  single tuple from larger resultset
      PGRES_SINGLE_TUPLE
     );

   function PQconnectdb (Conninfo : in chars_ptr) return PGconn_Access
     with Import => True, Convention => C, Link_Name => "PQconnectdb";

   function PQstatus (Conn : in PGconn_Access) return ConnStatusType
     with Import => True, Convention => C, Link_Name => "PQstatus";

   function PQerrorMessage (Conn : in PGconn_Access) return chars_ptr
     with Import => True, Convention => C, Link_Name => "PQerrorMessage";

   procedure PQfinish (Conn : PGconn_Access)
     with Import => True, Convention => C, Link_Name => "PQfinish";

   function PQexec (Conn : PGconn_Access;
                    SQL  : Interfaces.C.Strings.chars_ptr) return PGresult_Access
     with Import => True, Convention => C, Link_Name => "PQexec";

   procedure PQclear (Result : in PGresult_Access)
     with Import => True, Convention => C, Link_Name => "PQclear";

   function PQresultStatus (Result : in PGresult_Access) return ExecStatusType
     with Import => True, Convention => C, Link_Name => "PQresultStatus";

   function PQresultErrorMessage (Result : in PGresult_Access) return chars_ptr
     with Import => True, Convention => C, Link_Name => "PQresultErrorMessage";

   function PQcmdTuples (Result : in PGresult_Access) return chars_ptr
     with Import => True, Convention => C, Link_Name => "PQcmdTuples";

   function PQntuples (Result : in PGresult_Access) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "PQntuples";

   function PQnfields (Result : in PGresult_Access) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "PQnfields";

   function PQfname (Result : in PGresult_Access;
                     Column_Number : in Interfaces.C.int) return chars_ptr
     with Import => True, Convention => C, Link_Name => "PQfname";

   function PQfsize (Result : in PGresult_Access;
                     Column_Number : in Interfaces.C.int) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "PQfsize";

   function PQgetvalue (Result        : in PGresult_Access;
                        Row_Number    : in Interfaces.C.int;
                        Column_Number : in Interfaces.C.int) return chars_ptr
     with Import => True, Convention => C, Link_Name => "PQgetvalue";

   function PQgetisnull (Result        : in PGresult_Access;
                         Row_Number    : in Interfaces.C.int;
                         Column_Number : in Interfaces.C.int) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "PQgetisnull";

   function PQgetlength (Result        : in PGresult_Access;
                         Row_Number    : in Interfaces.C.int;
                         Column_Number : in Interfaces.C.int) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "PQgetlength";

end PQ;
