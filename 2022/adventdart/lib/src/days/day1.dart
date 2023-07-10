/*
 * Project: days
 * Created Date: Sunday July 9th 2023 3:20:57 pm
 * Author: Fa C Shus (paul@facshus.com)
 * -----
 * Last Modified: Sunday, 9th July 2023 3:20:57 pm
 * Modified By: Fa C Shus (paul@facshus.com)
 * -----
 * Copyright (c) 2021 - 2023 FaCShus Systems
 * License: MIT
 */

import 'package:adventdart/src/days/getdata.dart';
import 'package:mason_logger/mason_logger.dart';

(int, List<String>) sumCalories(Logger logger, List<String> data, int cals) {
  // logger.info('sumCalories: ${data.length}, $cals');
  switch (data) {
    case []:
      // logger.info('sumCalories: DONE EMPTY');
      return (cals, data);
    case _ when data[0] == '':
      return (cals, data);
    case _:
      final tail = data.sublist(1);
      return sumCalories(logger, tail, cals + int.parse(data[0]));
  }
}

(int, List<String>) findFatElf(Logger logger, List<String> data, int maxcals) {
  logger.info('findFatElf: ${data.length}, $maxcals');
  final (localmax, newData) = sumCalories(logger, data, 0);
  switch (localmax) {
    case 0:
      return (maxcals, data);
    case _ when newData.isEmpty:
      return (maxcals, data);
    case _:
      if (localmax > maxcals) {
        return findFatElf(logger, newData.sublist(1), localmax);
      } else {
        return findFatElf(
          logger,
          newData.sublist(1),
          maxcals,
        );
      }
  }
}

(int, int, int, List<String>) findFatElves(
  Logger logger,
  List<String> data,
  int elf1,
  int elf2,
  int elf3,
) {
  // logger.info('findFatElves: ${data.length}, $elf1, $elf2, $elf3');
  final (localmax, newData) = sumCalories(logger, data, 0);
  switch (localmax) {
    case 0:
      return (elf1, elf2, elf3, data);
    case _ when newData.isEmpty:
      return (elf1, elf2, elf3, data);
    case _ when localmax > elf1:
      return findFatElves(logger, newData.sublist(1), localmax, elf1, elf2);
    case _ when localmax > elf2:
      return findFatElves(logger, newData.sublist(1), elf1, localmax, elf2);
    case _ when localmax > elf3:
      return findFatElves(logger, newData.sublist(1), elf1, elf2, localmax);
    case _:
      return findFatElves(logger, newData.sublist(1), elf1, elf2, elf3);
  }
}

Future<void> runDay1(Logger logger, {int part = 1}) async {
  final input = await getData('../data/day-1');
  switch (part) {
    case 1:
      logger.info('Day 1 Part $part: ${findFatElf(logger, input, 0).$1}');
    case 2:
      final (elf1, elf2, elf3, _) = findFatElves(logger, input, 0, 0, 0);
      logger.detail('Day 1 Part $part: $elf1, $elf2, $elf3');
      logger.info('Day 1 Part $part: ${elf1 + elf2 + elf3}');
  }
}
