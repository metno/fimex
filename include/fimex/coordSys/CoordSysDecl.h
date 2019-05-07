/*
 * Fimex
 *
 * (C) Copyright 2019, met.no
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
 */

#ifndef COORDSYSDECL_H_
#define COORDSYSDECL_H_

#include <memory>

#include <vector>

namespace MetNoFimex {

class CoordinateSystem;
typedef std::shared_ptr<CoordinateSystem> CoordinateSystem_p;
typedef std::shared_ptr<const CoordinateSystem> CoordinateSystem_cp;
typedef std::vector<CoordinateSystem_cp> CoordinateSystem_cp_v;

class CoordinateAxis;
typedef std::shared_ptr<CoordinateAxis> CoordinateAxis_p;
typedef std::shared_ptr<const CoordinateAxis> CoordinateAxis_cp;
typedef std::vector<CoordinateAxis_cp> CoordinateAxis_cp_v;

class Projection;
typedef std::shared_ptr<Projection> Projection_p;
typedef std::shared_ptr<const Projection> Projection_cp;

class ToVLevelConverter;
typedef std::shared_ptr<ToVLevelConverter> ToVLevelConverter_p;

class VerticalConverter;
typedef std::shared_ptr<VerticalConverter> VerticalConverter_p;

class VerticalTransformation;
typedef std::shared_ptr<const VerticalTransformation> VerticalTransformation_cp;

} // namespace MetNoFimex

#endif /*COORDSYSDECL_H_*/
