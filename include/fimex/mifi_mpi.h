/*
 * Fimex, mifi_mpi.h
 *
 * (C) Copyright 2014, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Dec 22, 2014
 *      Author: heikok
 */


#include "mpi.h"

#ifndef MIFI_MPI_H_
#define MIFI_MPI_H_


#ifdef __cplusplus
extern "C" {
#endif

static MPI_Group mifi_mpi_group_world;
static MPI_Group mifi_mpi_grprem;
static MPI_Comm  mifi_mpi_commslave;
static int mifi_mpi_me;

#ifdef __cplusplus
}
#endif

#endif /* MIFI_MPI_H_ */
