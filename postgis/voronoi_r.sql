--this is the r_voronoi plr function adapted to postgis 2
/**
  This function uses the deldir library in R to generate voronoi polygons for
  an input set of points in a PostGIS table.
 
  Original function by Mike Leahy mgleahy at alumni.uwaterloo.ca
  http://postgis.refractions.net/pipermail/postgis-users/2007-June/016102.html
 
  Mar 6, 2012
  Cleaned up formatting, and updated a teensy bit to modern times
  Puneet Kishor punkish@eidesis.org
  All my contribution is released under a CC0 License Waiver
  Effectively in the Public Domain
 
  original Requirements:
      R-2.5.0 with deldir-0.0-5 installed
      PostgreSQL-8.2.x with PostGIS-1.x and PL/R-8.2.0.4 installed
  
  Requirements:
    R >= 2.5.0 with deldir >= 0.0-5 installed
    PostgreSQL 9.x with PostGIS 2.x and PL/R-9.x installed
 
  Usage: SELECT * FROM voronoi_r('table','point-column','id-column');
 
  Where:
  'table' is the table or query (see below) containing the points to be used
      for generating the voronoi polygons,
  'point-column' is a single 'POINT' PostGIS geometry type
      (each point must be unique)
  'id-column' is a unique identifying integer for each of the originating
      points (e.g., 'gid')
 
  Output: returns a recordset of the custom type 'voronoi', which contains 
      the id of the originating point, and a polygon geometry
 
  Alternative usage: Instead of the name of a table, pass a query that
      returns a result set. Make sure to us an ALIAS for the result set.
  SELECT * FROM r_voronoi(
      '(SELECT id, the_geom FROM table LIMIT 10) AS p',
      'p.the_geom',
      'p.id'
  )
*/
j
DROP FUNCTION if exists voronoi_r(text, text, text);
DROP TYPE if exists voronoi;
 
CREATE TYPE voronoi_r AS (id integer, polygon geometry);
 
CREATE OR REPLACE FUNCTION voronoi_r(text, text, text) RETURNS SETOF voronoi_r AS '

    library(deldir)
 
    # select the point x/y coordinates into a data frame
    points <- pg.spi.exec(
        sprintf(
            "SELECT ST_X(%2$s) AS x, ST_Y(%2$s) AS y FROM %1$s;",
            arg1,
            arg2
        )
    )
 
    # calculate an approprate buffer distance (~10%):
    buffer_distance = (
        (
            abs(max(points$x) - min(points$x)) +
            abs(max(points$y) - min(points$y))
        ) / 2
    ) * (0.10)
 
    # get EWKB for the overall buffer of the convex hull for all points:
    buffer_set <- pg.spi.exec(
        sprintf(
            "SELECT ST_Buffer(ST_Convexhull(ST_Union(%2$s)),%3$.6f) AS ewkb FROM %1$s;",
            arg1,
            arg2,
            buffer_distance
        )
    )
 
    # the following use of deldir uses high precision and digits to prevent
    # slivers between the output polygons, and uses a relatively large bounding
    # box with four dummy points included to ensure that points in the
    # peripheral areas of the dataset are appropriately enveloped by their
    # corresponding polygons:
    voro = deldir(
        points$x,
        points$y,
        digits=22,
        frac=0.001,
        list(ndx=2,ndy=2),
        rw=c(
            min(points$x) - abs(min(points$x) - max(points$x)),
            max(points$x) + abs(min(points$x) - max(points$x)),
            min(points$y) - abs(min(points$y) - max(points$y)),
            max(points$y) + abs(min(points$y) - max(points$y))
        )
    )
 
    tiles = tile.list(voro)
    poly = array()
    id = array()
    p = 1
 
    # construct the outgoing WKT now
    for (i in 1:length(tiles)) {
        tile = tiles[[i]]
 
        curpoly = "POLYGON(("
 
        for (j in 1:length(tile$x)) {
             curpoly = sprintf(
                "%s %.6f %.6f,",
                curpoly,
                tile$x[[j]],
                tile$y[[j]]
             )
        }
 
        curpoly = sprintf(
            "%s %.6f %.6f))",
            curpoly,
            tile$x[[1]],
            tile$y[[1]]
        )
 
        # this bit will find the original point that corresponds to the current
        # polygon, along with its id and the SRID used for the point geometry
        # (presumably this is the same for all points)...this will also filter
        # out the extra polygons created for the four dummy points, as they
        # will not return a result from this query:
        ipoint <- pg.spi.exec(
            sprintf(
                "SELECT %3$s AS id, st_intersection(''SRID=''||st_srid(%2$s)||'';%4$s'',''%5$s'') AS polygon FROM %1$s WHERE intersects(%2$s,''SRID=''||st_srid(%2$s)||'';%4$s'');",
                arg1,
                arg2,
                arg3,
                curpoly,
                buffer_set$ewkb[1]
            )
        )
        if (length(ipoint) > 0) {
            poly[[p]] <- ipoint$polygon[1]
            id[[p]] <- ipoint$id[1]
            p = (p + 1)
        }
    }
    return(data.frame(id,poly))
' language 'plr';
