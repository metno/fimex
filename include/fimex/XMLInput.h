/*
 * Fimex, XMLInput.h
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Oct 25, 2011
 *      Author: Heiko Klein
 */

#ifndef XMLINPUT_H_
#define XMLINPUT_H_

/**
 * @headerfile "fimex/XMLInput.h"
 */

#include <string>
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{

// forward decl.
class XMLDoc;

/**
 * @headerfile fimex/XMLInput.h
 */
/**
 * Interface for different XML sources like URL, file or string
 */
class XMLInput
{
public:
    virtual ~XMLInput() {}
    /**
     * retrieve the XMLDoc
     *
     * @return XMLDoc
     * @throw CDMException
     */
    virtual boost::shared_ptr<XMLDoc> getXMLDoc() const = 0;
    /**
     * return an identifier of the XMLInput
     */
    virtual std::string id() const = 0;
    /**
     * check if information is available
     */
    virtual bool isEmpty() const {return (id().empty());}
};

class XMLInputFile : public XMLInput
{
private:
    std::string filename_;
public:
    XMLInputFile(const std::string& filename) : filename_(filename) {}
    virtual boost::shared_ptr<XMLDoc> getXMLDoc() const;
    virtual std::string id() const {return filename_;}
};

class XMLInputString : public XMLInput
{
private:
    std::string content_;
    std::string url_;
public:
    /**
     * parse a in-memory xml-content. The url will be neede for external
     * references like style-definitions/-sheets or xincludes
     *
     */
    XMLInputString(const std::string& content, const std::string& url = "") : content_(content), url_(url) {}
    virtual boost::shared_ptr<XMLDoc> getXMLDoc() const;
    virtual std::string id() const {return content_.substr(0,((content_.size() > 100) ? 100 : content_.size()));}
};

class XMLInputDoc : public XMLInput
{
private:
    std::string id_;
    boost::shared_ptr<XMLDoc> doc_;
public:
    XMLInputDoc(const std::string& id, boost::shared_ptr<XMLDoc> doc) : id_(id), doc_(doc) {}
    virtual boost::shared_ptr<XMLDoc> getXMLDoc() const {return doc_;}
    virtual std::string id() const {return id_;}
};

} // namespace MetNoFimex

#endif /* XMLINPUT_H_ */
