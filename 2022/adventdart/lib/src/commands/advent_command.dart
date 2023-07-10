import 'package:adventdart/src/days/day1.dart';
import 'package:args/command_runner.dart';
import 'package:mason_logger/mason_logger.dart';

/// {@template sample_command}
///
/// `aoc sample`
/// A [Command] to exemplify a sub command
/// {@endtemplate}
class AdventCommand extends Command<int> {
  /// {@macro sample_command}
  AdventCommand({
    required Logger logger,
  }) : _logger = logger {
    argParser
      ..addOption(
        'day',
        abbr: 'd',
        allowed: [
          '1',
          '2',
          '3',
          '4',
          '5',
          '6',
          '7',
          '8',
          '9',
          '10',
          '11',
          '12',
          '13',
          '14',
          '15',
          '16',
          '17',
          '18',
          '19',
          '20',
          '21',
          '22',
          '23',
          '24',
          '25'
        ],
        help: 'The day to run',
        valueHelp: '1',
        defaultsTo: '1',
      )
      ..addOption(
        'part',
        abbr: 'p',
        allowed: ['1', '2'],
        help: 'The part of the day to run',
        valueHelp: '1',
        defaultsTo: '1',
        // )
        // ..addFlag(
        //   'cyan',
        //   abbr: 'c',
        //   help: 'Prints the same joke, but in cyan',
        //   negatable: false,
      );
  }

  @override
  String get description =>
      'The advent command that selects the day [part] to run';

  @override
  String get name => 'advent';

  final Logger _logger;

  @override
  Future<int> run() async {
    _logger
        .info('Running day ${argResults?['day']} part ${argResults?['part']}');
    switch (argResults?['day']) {
      case '1':
        runDay1(
          _logger,
          part: int.tryParse(argResults?['part'].toString() ?? '1') ?? 1,
        );
      // case '2':
      //   await runDay2();
      // case '3':
      //   await runDay3();
      // case '4':
      //   await runDay4();
      // case '5':
      //   await runDay5();
      // case '6':
      //   await runDay6();
      // case '7':
      //   await runDay7();
      // case '8':
      //   await runDay8();
      // case '9':
      //   await runDay9();
      // case '10':
      //   await runDay10();
      // case '11':
      //   await runDay11();
      // case '12':
      //   await runDay12();
      // case '13':
      //   await runDay13();
      // case '14':
      //   await runDay14();
      // case '15':
      //   await runDay15();
      // case '16':
      //   await runDay16();
      // case '17':
      //   await runDay17();
      // case '18':
      //   await runDay18();
      // case '19':
      //   await runDay19();
      // case '20':
      //   await runDay20();
      // case '21':
      //   await runDay21();
      // case '22':
      //   await runDay22();
      // case '23':
      //   await runDay23();
      // case '24':
      //   await runDay24();
      // case '25':
      //   await runDay25();
      default:
        await runDay1(
          _logger,
          part: int.tryParse(argResults?['part'].toString() ?? '1') ?? 1,
        );
    }
    // const output = 'Which unicorn has a cold? The Achoo-nicorn!';
    // // if (argResults?['cyan'] == true) {
    // //   output = lightCyan.wrap(output)!;
    // // }
    // _logger.info(output);
    return ExitCode.success.code;
  }
}
