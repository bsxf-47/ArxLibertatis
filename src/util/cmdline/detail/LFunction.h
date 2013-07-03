/*
 * Copyright 2013 Arx Libertatis Team (see the AUTHORS file)
 *
 * This file is part of Arx Libertatis.
 *
 * Original source is copyright 2010 - 2011. Alexey Tsoy.
 * http://sourceforge.net/projects/interpreter11/
 *
 * Boost Software License - Version 1.0 - August 17th, 2003
 *
 * Permission is hereby granted, free of charge, to any person or organization
 * obtaining a copy of the software and accompanying documentation covered by
 * this license (the "Software") to use, reproduce, display, distribute,
 * execute, and transmit the Software, and to prepare derivative works of the
 * Software, and to permit third-parties to whom the Software is furnished to
 * do so, all subject to the following:
 *
 * The copyright notices in the Software and this entire statement, including
 * the above license grant, this restriction and the following disclaimer,
 * must be included in all copies of the Software, in whole or in part, and
 * all derivative works of the Software, unless such copies or derivative
 * works are solely in the form of machine-executable object code generated by
 * a source language processor.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
 * FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifndef ARX_UTIL_CMDLINE_DETAIL_LFUNCTION_H
#define ARX_UTIL_CMDLINE_DETAIL_LFUNCTION_H

#include <boost/config.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/comma_if.hpp>
#include <boost/preprocessor/enum_params.hpp>
#include <boost/preprocessor/repeat_from_to.hpp>
#include <boost/preprocessor/iteration/iterate.hpp>

namespace util { namespace cmdline {

namespace detail {
	template<typename FnSign>
	struct lfn_invoker;
} // namespace detail

template<typename FnSign, typename Function>
struct lfunction {
	typedef FnSign                          signature;
	typedef detail::lfn_invoker<signature>  lfn_invoker;
	
	Function function;
	
	explicit lfunction(Function const& function) : function(function) {
	}
	
	template<typename Args>
	typename lfn_invoker::result_type operator()(Args & args) {
		return lfn_invoker()(function,args);
	}
};

template<typename FnSign, typename Function>
lfunction<FnSign,Function> make_lfunction(Function const& fn) {
	return lfunction<FnSign,Function>(fn);
}

#define BOOST_PP_FILENAME_1                  "util/cmdline/detail/lfunction/Preprocessed.h"
#define BOOST_COMMAND_LINE_MAX_FUNCTION_ARGS 10
#define BOOST_PP_ITERATION_LIMITS            (0, BOOST_COMMAND_LINE_MAX_FUNCTION_ARGS)

#include BOOST_PP_ITERATE()

#undef BOOST_PP_FILENAME_1
#undef BOOST_COMMAND_LINE_MAX_FUNCTION_ARGS
#undef BOOST_PP_ITERATION_LIMITS

} } // namespace util::cmdline

#endif // ARX_UTIL_CMDLINE_DETAIL_LFUNCTION_H
