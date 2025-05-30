/* LIBGIMP - The GIMP Library
 * Copyright (C) 1995-2003 Peter Mattis and Spencer Kimball
 *
 * gimpgradientselect_pdb.c
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <https://www.gnu.org/licenses/>.
 */

/* NOTE: This file is auto-generated by pdbgen.pl */

#include "config.h"

#include "stamp-pdbgen.h"

#include "gimp.h"


/**
 * SECTION: gimpgradientselect
 * @title: gimpgradientselect
 * @short_description: Methods of a gradient chooser dialog
 *
 * A dialog letting a user choose a gradient.  Read more at
 * gimpfontselect.
 **/


/**
 * gimp_gradients_popup:
 * @gradient_callback: The callback PDB proc to call when user chooses a gradient.
 * @popup_title: Title of the gradient selection dialog.
 * @initial_gradient: (nullable): The initial gradient choice.
 * @parent_window: (nullable): An optional parent window handle for the popup to be set transient to.
 *
 * Invokes the Gimp gradients selection dialog.
 *
 * Opens a dialog letting a user choose a gradient.
 *
 * Returns: TRUE on success.
 **/
gboolean
gimp_gradients_popup (const gchar  *gradient_callback,
                      const gchar  *popup_title,
                      GimpGradient *initial_gradient,
                      GBytes       *parent_window)
{
  GimpValueArray *args;
  GimpValueArray *return_vals;
  gboolean success = TRUE;

  args = gimp_value_array_new_from_types (NULL,
                                          G_TYPE_STRING, gradient_callback,
                                          G_TYPE_STRING, popup_title,
                                          GIMP_TYPE_GRADIENT, initial_gradient,
                                          G_TYPE_BYTES, parent_window,
                                          G_TYPE_NONE);

  return_vals = _gimp_pdb_run_procedure_array (gimp_get_pdb (),
                                               "gimp-gradients-popup",
                                               args);
  gimp_value_array_unref (args);

  success = GIMP_VALUES_GET_ENUM (return_vals, 0) == GIMP_PDB_SUCCESS;

  gimp_value_array_unref (return_vals);

  return success;
}

/**
 * gimp_gradients_close_popup:
 * @gradient_callback: The name of the callback registered for this pop-up.
 *
 * Close the gradient selection dialog.
 *
 * Closes an open gradient selection dialog.
 *
 * Returns: TRUE on success.
 **/
gboolean
gimp_gradients_close_popup (const gchar *gradient_callback)
{
  GimpValueArray *args;
  GimpValueArray *return_vals;
  gboolean success = TRUE;

  args = gimp_value_array_new_from_types (NULL,
                                          G_TYPE_STRING, gradient_callback,
                                          G_TYPE_NONE);

  return_vals = _gimp_pdb_run_procedure_array (gimp_get_pdb (),
                                               "gimp-gradients-close-popup",
                                               args);
  gimp_value_array_unref (args);

  success = GIMP_VALUES_GET_ENUM (return_vals, 0) == GIMP_PDB_SUCCESS;

  gimp_value_array_unref (return_vals);

  return success;
}

/**
 * gimp_gradients_set_popup:
 * @gradient_callback: The name of the callback registered for this pop-up.
 * @gradient: The gradient to set as selected.
 *
 * Sets the current gradient in a gradient selection dialog.
 *
 * Sets the current gradient in a gradient selection dialog.
 *
 * Returns: TRUE on success.
 **/
gboolean
gimp_gradients_set_popup (const gchar  *gradient_callback,
                          GimpGradient *gradient)
{
  GimpValueArray *args;
  GimpValueArray *return_vals;
  gboolean success = TRUE;

  args = gimp_value_array_new_from_types (NULL,
                                          G_TYPE_STRING, gradient_callback,
                                          GIMP_TYPE_GRADIENT, gradient,
                                          G_TYPE_NONE);

  return_vals = _gimp_pdb_run_procedure_array (gimp_get_pdb (),
                                               "gimp-gradients-set-popup",
                                               args);
  gimp_value_array_unref (args);

  success = GIMP_VALUES_GET_ENUM (return_vals, 0) == GIMP_PDB_SUCCESS;

  gimp_value_array_unref (return_vals);

  return success;
}
