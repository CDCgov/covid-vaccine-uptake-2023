# cfa-covid-booster-uptake-projection

In-season projections of COVID-19 vaccine uptake for the 2023/2024 season.

## Project Admins

Scott Olesen <ulp7@cdc.gov> (CDC/IOD/ORR/CFA)
Inga Holmdahl <usn4@cdc.gov> (CDC/IOD/ORR/CFA)

## Scenarios

We have generated three different methods that may give us an idea of the trajectory we are on within the season (2023/2024), in relation to a reference season (2022/2023).

- Date-based: Assume that the primary determinant of uptake is the week of the year; continue uptake from the same calendar *date* of the reference season year, regardless of how uptake compares so far
- Rate-based: Assume that the rate of uptake will decrease over time, in a similar way to the reference season. Continue uptake at the *rate* of the reference season, starting from the week whose rate best matches the current rate
- Sentiment-based: Assume that of people stating they intend to get vaccinated at the beginning of the current season, the proportion who actually will get vaccinated is constant between seasons

We do not assume that any of these is an exact reflection of the many factors driving uptake, but use them methods to project possible uptake scenarios over the coming season.

All three of these methods are designed to be used after vaccine rollout has begun, but before the season has ended. They each are desgined to use only a single previous season as reference.


## Workflow

To produce plots of 2023/2024 uptake projections weekly throughout the 2023/2024 season, with 2022/2023 uptake as a reference:

1. `devtools::load_all("uptakeprojection")`
2. `source("scripts/setup.R")`
3. `source("scripts/project_uptake.R")`

## Data sources

### Public data

We refer to [data.cdc.gov](https://data.cdc.gov/) sources, which this packages pulls from the public API:

- `udsf-9v7b` - 2022/2023 NIS
- `pakc-hru3` - 2023/2024 NIS

## General Disclaimer

This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/organization/mission.htm).  GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise.

## Public Domain Standard Notice

This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice

This repository is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice

This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md)
and [Code of Conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice

Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice

This repository is not a source of government records but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices

Please refer to [CDC's Template Repository](https://github.com/CDCgov/template)
for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/master/CONTRIBUTING.md),
[public domain notices and disclaimers](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md),
and [code of conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
