/* LIBGIMP - The GIMP Library
 * Copyright (C) 1995-1997 Peter Mattis and Spencer Kimball
 *
 * gimpcellrenderercolor.h
 * Copyright (C) 2004  Sven Neuman <sven@gimp.org>
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <https://www.gnu.org/licenses/>.
 */

#pragma once

#if !defined (__GIMP_WIDGETS_H_INSIDE__) && !defined (GIMP_WIDGETS_COMPILATION)
#error "Only <libgimpwidgets/gimpwidgets.h> can be included directly."
#endif

G_BEGIN_DECLS

#define GIMP_TYPE_CELL_RENDERER_COLOR (gimp_cell_renderer_color_get_type ())
G_DECLARE_FINAL_TYPE (GimpCellRendererColor,
                      gimp_cell_renderer_color,
                      GIMP, CELL_RENDERER_COLOR,
                      GtkCellRenderer)


GtkCellRenderer * gimp_cell_renderer_color_new (void);

G_END_DECLS
